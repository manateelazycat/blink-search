#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
import queue
import threading
import traceback

from epc.server import ThreadingEPCServer
from core.utils import *

from backend.search_elisp_symbol import SearchElispSymbol
from backend.search_recent_file import SearchRecentFile
from backend.search_buffer_list import SearchBufferList
from backend.search_eaf_browser_history import SearchEAFBrowserHistory
from backend.search_google_suggestion import SearchGoogleSuggestion
from backend.search_fd import SearchFd
from backend.search_rg import SearchRg
from backend.search_current_buffer import SearchCurrentBuffer

class BlinkSearch:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))
        
        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(blink-search_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()
        
        # Init emacs option.
        enable_lsp_server_log = get_emacs_var("blink-search-enable-log")
        if enable_lsp_server_log:
            logger.setLevel(logging.DEBUG)

        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()
        
        # All backend response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()
        
        # Init search item dict.
        self.search_dict = {}
        self.search_row_number = 0
        self.search_candidate_items = []
        self.search_backend_items = []
        self.render_candidate_index = 0
        self.render_backend_index = 0
        self.render_candidate_items = []
        self.render_backend_items = []
        self.render_candidate_offset = 0
        self.render_backend_offset = 0
        
        # Init search backend.
        self.search_elisp_symbol = SearchElispSymbol("Elisp Symbol", self.message_queue)
        self.search_recent_file = SearchRecentFile("Recent File", self.message_queue)
        self.search_buffer_list = SearchBufferList("Buffer List", self.message_queue)
        self.search_eaf_browser_history = SearchEAFBrowserHistory("EAF Browser History", self.message_queue)
        self.search_google_suggestion = SearchGoogleSuggestion("Google Suggest", self.message_queue)
        self.search_fd = SearchFd("Find File", self.message_queue)
        self.search_rg = SearchRg("Grep File", self.message_queue)
        self.search_current_buffer = SearchCurrentBuffer("Current Buffer", self.message_queue)
        
        # Pass epc port and webengine codec information to Emacs when first start blink-search.
        eval_in_emacs('blink-search--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                message = self.event_queue.get(True)
                self.message_handler(message)
                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())
            
    def message_dispatcher(self):
        try:
            while True:
                message = self.message_queue.get(True)
                
                self.message_handler(message)
            
                self.message_queue.task_done()
        except:
            logger.error(traceback.format_exc())
            
    def message_handler(self, message):
        if message["name"] == "update_backend_items":
            self.search_dict[message["backend"]] = message["items"]
            
            self.search_candidate_items = []
            self.search_backend_items = []
            
            candidate_items = []
            for backend_info in [
                    ["Buffer List", 5],
                    ["Find File", 5],
                    ["Recent File", 5],
                    ["EAF Browser History", 5],
                    ["Current Buffer", 5],
                    ["Grep File", 5],
                    ["Elisp Symbol", 5],
                    ["Google Suggest", 3]]:
                [backend_name, candidate_show_number] = backend_info
                if backend_name in self.search_dict and self.search_dict[backend_name] != None and len(self.search_dict[backend_name]) > 0:
                    candidates = self.search_dict[backend_name]
                    for candidate in candidates[:min(len(candidates), candidate_show_number)]:
                        candidate_items.append({
                            "backend": backend_name,
                            "candidate": candidate,
                            "number": len(candidates)
                        })
                    
            if len(candidate_items) > 0:
                backend_items = self.search_dict[candidate_items[0]["backend"]]
                
                self.search_candidate_items = candidate_items
                self.search_backend_items = backend_items
                
                self.render_candidate_items = candidate_items[:min(self.search_row_number, len(candidate_items))]
                self.render_backend_items = backend_items[:min(self.search_row_number, len(backend_items))]
                self.render_candidate_offset = 0
                self.render_candidate_index = 0
                self.render_backend_offset = 0
                self.render_backend_index = 0
                
                eval_in_emacs("blink-search-update-items", 
                              self.render_candidate_items, self.render_candidate_index, 
                              self.render_backend_items, self.render_backend_index)
            
    def adjust_backend_item_with_candidate_status(self):
        try:
            candiate = self.search_candidate_items[self.render_candidate_offset + self.render_candidate_index]
            backend_index = self.search_backend_items.index(candiate["candidate"])
            
            if backend_index >= self.search_row_number:
                self.render_backend_offset = backend_index - (self.search_row_number - 1)
                self.render_backend_index = self.search_row_number - 1
            else:
                self.render_backend_offset = 0
                self.render_backend_index = backend_index
                
            self.render_backend_items = self.search_backend_items[self.render_backend_offset:min(self.search_row_number, len(self.search_backend_items))]
        except:
            import traceback
            traceback.print_exc()
            
            self.render_backend_offset = 0
            self.render_backend_index = 0
            
            self.render_backend_items = self.search_backend_items[:min(self.search_row_number, len(self.search_backend_items))]
            
    def select_next_candidate_item(self):
        need_update = True
        
        if len(self.search_candidate_items) > 0:
            if self.render_candidate_index < min(self.search_row_number, len(self.search_candidate_items)) - 1:
                self.render_candidate_index += 1
                self.search_backend_items = self.search_dict[self.render_candidate_items[self.render_candidate_index]["backend"]]
            elif self.render_candidate_offset + self.render_candidate_index == len(self.search_candidate_items) - 1:
                need_update = False
            else:
                self.render_candidate_offset += 1
                self.render_candidate_items = self.search_candidate_items[self.render_candidate_offset:self.render_candidate_offset + self.search_row_number]
                self.search_backend_items = self.search_dict[self.render_candidate_items[self.render_candidate_index]["backend"]]
        else:
            need_update = False
        
        if need_update:
            self.adjust_backend_item_with_candidate_status()
            
            eval_in_emacs("blink-search-update-items", 
                          self.render_candidate_items, self.render_candidate_index, 
                          self.render_backend_items, self.render_backend_index)

    def select_prev_candidate_item(self):
        need_update = True
        
        if len(self.search_candidate_items) > 0:
            if self.render_candidate_index > 0:
                self.render_candidate_index -= 1
                self.render_candidate_items = self.search_candidate_items[self.render_candidate_offset:self.render_candidate_offset + self.search_row_number]
                self.search_backend_items = self.search_dict[self.render_candidate_items[self.render_candidate_index]["backend"]]
            elif self.render_candidate_offset == 0 and self.render_candidate_index == 0:
                need_update = False
            else:
                self.render_candidate_offset -= 1
                self.render_candidate_items = self.search_candidate_items[self.render_candidate_offset:self.render_candidate_offset + self.search_row_number]
                self.search_backend_items = self.search_dict[self.render_candidate_items[self.render_candidate_index]["backend"]]
        else:
            need_update = False
        
        if need_update:
            self.adjust_backend_item_with_candidate_status()
            
            eval_in_emacs("blink-search-update-items", 
                          self.render_candidate_items, self.render_candidate_index, 
                          self.render_backend_items, self.render_backend_index)
    
    def select_next_backend_item(self):
        need_update = True
        
        if len(self.search_backend_items) > 0:
            if self.render_backend_index < min(self.search_row_number, len(self.search_backend_items)) - 1:
                self.render_backend_index += 1
            elif self.render_backend_offset + self.render_backend_index == len(self.search_backend_items) - 1:
                need_update = False
            else:
                self.render_backend_offset += 1
                self.render_backend_items = self.search_backend_items[self.render_backend_offset:self.render_backend_offset + self.search_row_number]
        else:
            need_update = False
        
        if need_update:
            eval_in_emacs("blink-search-update-items", 
                          self.render_candidate_items, self.render_candidate_index, 
                          self.render_backend_items, self.render_backend_index)

    def select_prev_backend_item(self):
        need_update = True
        
        if len(self.search_backend_items) > 0:
            if self.render_backend_index > 0:
                self.render_backend_index -= 1
            elif self.render_backend_offset == 0 and self.render_backend_index == 0:
                need_update = False
            else:
                self.render_backend_offset -= 1
                self.render_backend_items = self.search_backend_items[self.render_backend_offset:self.render_backend_offset + self.search_row_number]
        else:
            need_update = False
        
        if need_update:
            eval_in_emacs("blink-search-update-items", 
                          self.render_candidate_items, self.render_candidate_index, 
                          self.render_backend_items, self.render_backend_index)
    
    def search_elisp_symbol_update(self, symbols):
        self.search_elisp_symbol.update(symbols)
        
    def search_recent_file_update(self, files):
        self.search_recent_file.update(files)
        
    def search_sort_buffer_list_update(self, buffers):
        self.search_buffer_list.update_sort_buffers(buffers)

    def search_buffer_list_update(self, buffers):
        self.search_buffer_list.update(buffers)
        
    def search_init_start_dir(self, start_dir):
        self.search_fd.init_dir(start_dir)
        self.search_rg.init_dir(start_dir)
        
    def search_init_buffer(self, buffer_name, buffer_content):
        self.search_current_buffer.init_buffer(buffer_name, buffer_content)
        
    def search_do(self, backend, candidate):
        if backend == "Find File":
            self.search_fd.do(candidate)
        elif backend == "Grep File":
            self.search_rg.do(candidate)
        elif backend == "Google Suggest":
            self.search_google_suggestion.do(candidate)
        elif backend == "Current Buffer":
            self.search_current_buffer.do(candidate)
        
    def search(self, input, row_number):
        self.search_row_number = row_number
        
        self.search_elisp_symbol.search(input)
        self.search_recent_file.search(input)
        self.search_buffer_list.search(input)
        self.search_eaf_browser_history.search(input)
        self.search_google_suggestion.search(input)
        self.search_fd.search(input)
        self.search_rg.search(input)
        self.search_current_buffer.search(input)
        
    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

if __name__ == "__main__":
    BlinkSearch(sys.argv[1:])
