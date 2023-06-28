#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
# 
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: <lazycat.manatee@gmail.com> <lazycat.manatee@gmail.com>
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

import threading
import re
import subprocess

from core.utils import message_emacs, eval_in_emacs


class Search:
    
    def __init__(self, backend_name, message_queue) -> None:
        self.backend_name = backend_name
        self.search_ticker = 0
        self.search_thread_queue = []
        self.items = []
        
        self.message_queue = message_queue
        
        self.sub_process = None
        
    def search(self, prefix: str):
        ticker = self.search_ticker + 1
        self.search_ticker = ticker
        
        search_thread = threading.Thread(target=lambda: self.search_items(prefix, ticker))
        search_thread.start()
        self.search_thread_queue.append(search_thread)
        
    def search_items(self, prefix: str, ticker: int):
        candidates = self.search_match(prefix)
            
        if ticker == self.search_ticker:
            self.message_queue.put({
                "name": "update_backend_items",
                "backend": self.backend_name,
                "items": candidates,
                "keyword": prefix
            })
            
    def update(self, items):
        self.items = sorted(items, key=len)
        
    def is_match(self, prefix, prefix_regexp, symbol):
        return symbol.startswith(prefix) or symbol.replace("-", "").startswith(prefix) or prefix in symbol or prefix_regexp.match(symbol)
    
    def search_match(self, prefix):
        prefix = prefix.replace("*", "")
        prefix_regexp = re.compile(".*" + ".*".join(prefix.split()), re.IGNORECASE)
        return list(filter(lambda symbol: self.is_match(prefix, prefix_regexp, symbol), self.items))
    
    def get_process_result(self, command_list, cwd=None):
        self.kill_sub_process()
            
        self.sub_process = subprocess.Popen(command_list, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding="utf-8")
        
        results = []
        try:
            outs, errs = self.sub_process.communicate()
            results = list(map(lambda p: p.strip(), outs.splitlines()))    # type: ignore
        except:
            import traceback
            traceback.print_exc()
            self.kill_sub_process()
            
        return results
            
    def select(self, candidate, start_buffer_name):
        eval_in_emacs("blink-search-select-start-buffer", start_buffer_name)
        
    def copy(self, candidate):
        eval_in_emacs("kill-new", candidate)
        message_emacs("Copy: {}".format(candidate))

    def do(self, candidate):
        pass
        
    def parent(self, candidate):
        self.do(candidate)
        
    def kill_sub_process(self):
        if self.sub_process is not None:
            try:
                self.sub_process.kill()
            except:
                pass
            
            self.sub_process = None
        
    def clean(self):
        self.kill_sub_process()
