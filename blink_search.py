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

import queue
import threading
import traceback

from epc.server import ThreadingEPCServer
from core.utils import *

from backend.search_elisp_symbol import SearchElispSymbol
from backend.search_recent_file import SearchRecentFile
from backend.search_buffer_list import SearchBufferList
from backend.search_eaf_browser_history import SearchEAFBrowserHistory
from backend.search_google_suggest import SearchGoogleSuggest
from backend.search_find_file import SearchFindFile
from backend.search_grep_file import SearchGrepFile
from backend.search_current_buffer import SearchCurrentBuffer
from backend.search_imenu import SearchIMenu
from backend.search_common_directory import SearchCommonDirectory
from backend.search_key_value_store import SearchKeyValueStore
from backend.search_grep_pdf import SearchGrepPDF

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
        self.search_start_buffer_name = ""
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
        self.search_google_suggestion = SearchGoogleSuggest("Google Suggest", self.message_queue)
        self.search_find_file = SearchFindFile("Find File", self.message_queue)
        self.search_grep_file = SearchGrepFile("Grep File", self.message_queue)
        self.search_current_buffer = SearchCurrentBuffer("Current Buffer", self.message_queue)
        self.search_imenu = SearchIMenu("IMenu", self.message_queue)
        self.search_common_directory = SearchCommonDirectory("Common Directory", self.message_queue)
        self.search_key_value_store = SearchKeyValueStore("Key Value", self.message_queue)
        self.search_grep_pdf = SearchGrepPDF("Grep PDF", self.message_queue)
        self.search_keyword = ""
        
        self.search_backend_dict = {}
        for backend in [self.search_elisp_symbol, self.search_recent_file, self.search_buffer_list,
                        self.search_eaf_browser_history, self.search_google_suggestion, self.search_common_directory,
                        self.search_find_file, self.search_grep_file, self.search_current_buffer, self.search_imenu,
                        self.search_key_value_store, self.search_grep_pdf]:
            self.search_backend_dict[backend.backend_name] = backend
            
            # Build backend update API.
            self.build_update_interface(str(backend).split(".")[1])
        self.search_backend_list = []
        self.search_backend_default_list = [
            "Buffer List", "Common Directory", "Find File", "Recent File", "EAF Browser History", 
            "IMenu", "Elisp Symbol", "Google Suggest", "Key Value"
        ]
        
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
            
    def backend_candidate_number(self, backend_name):
        try:
            return len(self.search_dict[backend_name])
        except:
            return 0
            
    def message_handler(self, message):
        if message["name"] == "update_backend_items":
            self.search_dict[message["backend"]] = message["items"]
            
            self.search_candidate_items = []
            self.search_backend_items = []
            
            # Get counter of all candidate.
            candidate_counter = sum(list(map(lambda backend_name: self.backend_candidate_number(backend_name), self.search_backend_list)))
            
            candidate_items = []
            for backend_name in self.search_backend_list:
                if backend_name in self.search_dict and self.search_dict[backend_name] is not None and len(self.search_dict[backend_name]) > 0:
                    candidates = self.search_dict[backend_name]
                    if len(self.search_backend_list) > 1:
                        if candidate_counter < self.search_row_number:
                            # Show all candidates if total number is less than row number.
                            candidate_show_number = len(candidates)
                        else:
                            # Show as many candidates as possible.
                            candidate_show_number = max(5, int(self.search_row_number / candidate_counter))
                    else:
                        # Show all candidates if only have one backend.
                        candidate_show_number = len(candidates)
                        
                    for candidate in candidates[:min(len(candidates), candidate_show_number)]:
                        candidate_items.append({
                            "backend": backend_name,
                            "candidate": candidate
                        })
                    
            if len(candidate_items) > 0:
                backend_items = self.search_dict[candidate_items[0]["backend"]]
                
                self.search_candidate_items = candidate_items
                self.search_backend_items = backend_items
                
                if message["keyword"] != self.search_keyword:
                    # Move cursor to first candidate when search keyword change.
                    self.render_candidate_items = candidate_items[:min(self.search_row_number, len(candidate_items))]
                    self.render_backend_items = backend_items[:min(self.search_row_number, len(backend_items))]
                    self.render_candidate_offset = 0
                    self.render_candidate_index = 0
                    self.render_backend_offset = 0
                    self.render_backend_index = 0
                else:
                    # Don't change candidate offset if search keyword not change.
                    self.update_render_candidate_items()
                    self.update_render_index_and_offset()
            else:
                # Clean everything if no result found.
                backend_items = []
                
                self.search_candidate_items = []
                self.search_backend_items = []
                
                self.render_candidate_items = []
                self.render_backend_items = []
                self.render_candidate_offset = 0
                self.render_candidate_index = 0
                self.render_backend_offset = 0
                self.render_backend_index = 0
                
            self.search_keyword = message["keyword"]
                
            self.render_items()
                
    def render_items(self):
        if len(self.search_candidate_items) > 0:
            backend_name = self.search_candidate_items[self.render_candidate_offset + self.render_candidate_index]["backend"] 
            candidate_index = self.render_backend_offset + self.render_backend_index + 1 
        else:
            backend_name = ""
            candidate_index = 0
        
        eval_in_emacs("blink-search-update-items", 
                      self.render_candidate_items, self.render_candidate_index, 
                      self.render_backend_items, self.render_backend_index,
                      backend_name, candidate_index,
                      len(self.search_backend_items), len(self.search_backend_list))
                
    def update_render_index_and_offset(self):
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
            
    def update_render_candidate_items(self):
        self.render_candidate_items = self.search_candidate_items[self.render_candidate_offset:self.render_candidate_offset + self.search_row_number]
        self.search_backend_items = self.search_dict[self.render_candidate_items[self.render_candidate_index]["backend"]]
            
    def update_render_backend_items(self):
        self.render_backend_items = self.search_backend_items[self.render_backend_offset:self.render_backend_offset + self.search_row_number]
        
    def select_candidate_item(self):
        try:
            candiate_item = self.search_candidate_items[self.render_candidate_offset + self.render_candidate_index]
            backend_name = candiate_item["backend"]
            candidate = candiate_item["candidate"]
            
            self.search_backend_dict[backend_name].select(candidate, self.search_start_buffer_name)
        except:
            pass
        
    def get_candiate_group_list(self):
        group_list = []
        last_backend_name = ""
        backend_names = list(map(lambda i: i["backend"], self.search_candidate_items))
        for index, backend_name in enumerate(backend_names):
            if backend_name != last_backend_name:
                group_list.append(index)
                last_backend_name = backend_name
                
        return group_list
    
    def select_backend_item(self):
        try:
            candiate_item = self.search_candidate_items[self.render_candidate_offset + self.render_candidate_index]
            backend_name = candiate_item["backend"]
            
            candidate = self.search_backend_items[self.render_backend_offset + self.render_backend_index]
            
            self.search_backend_dict[backend_name].select(candidate, self.search_start_buffer_name)
        except:
            pass
        
    def select_next_candidate_item(self):
        need_update = True
        
        if len(self.search_candidate_items) > 0:
            if self.render_candidate_index < min(self.search_row_number, len(self.search_candidate_items)) - 1:
                self.render_candidate_index += 1
                self.update_render_candidate_items()
            elif self.render_candidate_offset + self.render_candidate_index == len(self.search_candidate_items) - 1:
                need_update = False
            else:
                self.render_candidate_offset += 1
                self.update_render_candidate_items()
        else:
            need_update = False
        
        if need_update:
            self.update_render_index_and_offset()
            self.render_items()
            
    def select_prev_candidate_item(self):
        need_update = True
        
        if len(self.search_candidate_items) > 0:
            if self.render_candidate_index > 0:
                self.render_candidate_index -= 1
                self.update_render_candidate_items()
            elif self.render_candidate_offset == 0 and self.render_candidate_index == 0:
                need_update = False
            else:
                self.render_candidate_offset -= 1
                self.update_render_candidate_items()
        else:
            need_update = False
        
        if need_update:
            self.update_render_index_and_offset()
            self.render_items()
            
    def select_next_candidate_group(self):
        if len(self.search_candidate_items) > 0:
            candidate_index = self.render_candidate_offset + self.render_candidate_index
            group_list = self.get_candiate_group_list()
            match_list = list(filter(lambda index: candidate_index < index, group_list))
            if len(match_list) > 0:
                next_index = match_list[0]
                
                if self.render_candidate_index == 0 and self.render_candidate_offset == 0:
                    self.render_candidate_index = next_index
                elif next_index >= self.render_candidate_offset and next_index < self.render_candidate_offset + self.search_row_number:
                    self.render_candidate_index = next_index - self.render_candidate_offset
                else:
                    self.render_candidate_index = self.search_row_number - 1
                    self.render_candidate_offset = next_index - (self.search_row_number - 1)
                    
                self.update_render_candidate_items()
                self.update_render_index_and_offset()
                self.render_items()
    
    def select_prev_candidate_group(self):
        if len(self.search_candidate_items) > 0:
            candidate_index = self.render_candidate_offset + self.render_candidate_index
            group_list = self.get_candiate_group_list()
            match_list = list(filter(lambda index: candidate_index > index, group_list))
            if len(match_list) > 0:
                next_index = match_list[-1]
                
                if self.render_candidate_index == 0 and self.render_candidate_offset == 0:
                    self.render_candidate_index = next_index
                elif next_index >= self.render_candidate_offset and next_index < self.render_candidate_offset + self.search_row_number:
                    self.render_candidate_index = next_index - self.render_candidate_offset
                else:
                    self.render_candidate_offset = next_index
                    self.render_candidate_index = 0
                    
                self.update_render_candidate_items()
                self.update_render_index_and_offset()
                self.render_items()
            
    def select_next_backend_item(self):
        need_update = True
        
        if len(self.search_backend_items) > 0:
            if self.render_backend_index < min(self.search_row_number - 1, len(self.search_backend_items)) - 1:
                self.render_backend_index += 1
            elif self.render_backend_offset + self.render_backend_index == len(self.search_backend_items) - 1:
                need_update = False
            else:
                self.render_backend_offset += 1
                self.update_render_backend_items()
        else:
            need_update = False
        
        if need_update:
            self.render_items()
            self.select_backend_item()

    def select_prev_backend_item(self):
        need_update = True
        
        if len(self.search_backend_items) > 0:
            if self.render_backend_index > 0:
                self.render_backend_index -= 1
            elif self.render_backend_offset == 0 and self.render_backend_index == 0:
                need_update = False
            else:
                self.render_backend_offset -= 1
                self.update_render_backend_items()
        else:
            need_update = False
        
        if need_update:
            self.render_items()
            self.select_backend_item()
    
    def build_update_interface(self, backend_name):
        def _do(*args):
            getattr(self, backend_name).update(*args)

        setattr(self, "{}_update".format(backend_name), _do)
            
    def search_sort_buffer_list_update(self, buffers):
        self.search_buffer_list.update_sort_buffers(buffers)

    def search_init_search_dir(self, start_dir):
        for backend in self.search_backend_dict:
            backend_var_name = str(self.search_backend_dict[backend]).split(".")[1]
            if hasattr(self, backend_var_name):
                backend_var = getattr(self, backend_var_name)
                if hasattr(backend_var, "init_dir"):
                    getattr(backend_var, "init_dir")(start_dir)
        
    def search_init_current_buffer(self, buffer_name, buffer_content):
        self.search_start_buffer_name = buffer_name
        self.search_current_buffer.init_buffer(buffer_name, buffer_content)
        
    def search_do(self, backend, candidate):
        self.search_backend_dict[backend].do(candidate)
        
    def search_copy(self, backend, candidate):
        self.search_backend_dict[backend].copy(candidate)
        
    def search_parent(self, backend, candidate):
        self.search_backend_dict[backend].parent(candidate)

    def search_continue(self, backend, candidate):
        backend = self.search_backend_dict[backend]
        if hasattr(backend, "continue_search"):
            getattr(backend, "continue_search")(candidate)
        else:
            message_emacs("{} not support continue search.".format(backend.backend_name))
        
    def search(self, input, row_number, backend_list):
        self.search_row_number = row_number
        
        if len(backend_list) == 0:
            self.search_backend_list = self.search_backend_default_list
        else:
            self.search_backend_list = backend_list
        
        for backend in self.search_backend_list:
            self.search_backend_dict[backend].search(input)
            
    def clean(self):
        for k, v in self.search_backend_dict.items():
            if hasattr(v, "clean"):
                getattr(v, "clean")()
        
    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

if __name__ == "__main__":
    BlinkSearch(sys.argv[1:])
