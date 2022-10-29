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
import os
import traceback
import sexpdata
import re

from core.utils import get_emacs_vars, message_emacs, eval_in_emacs, logger    # type: ignore


class Search:
    
    def __init__(self, backend_name, message_queue) -> None:
        self.backend_name = backend_name
        self.search_ticker = 0
        self.search_thread_queue = []
        self.items = []
        
        self.message_queue = message_queue
        
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
                "items": candidates
            })
            
    def update(self, items):
        self.items = sorted(items, key=len)
        
    def is_match(self, prefix, prefix_regexp, symbol):
        return False
    
    def search_match(self, prefix):
        pass
