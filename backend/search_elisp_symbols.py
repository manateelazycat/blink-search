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


class SearchElispSymbols:
    
    def __init__(self, message_queue) -> None:
        self.search_ticker = 0
        self.search_thread_queue = []
        self.symbols = []
        
        self.message_queue = message_queue
        
    def update(self, symbols):
        self.symbols = sorted(symbols, key=len)
        
    def search(self, prefix: str):
        ticker = self.search_ticker + 1
        self.search_ticker = ticker
        
        search_thread = threading.Thread(target=lambda: self.search_symbols(prefix, ticker))
        search_thread.start()
        self.search_thread_queue.append(search_thread)
        
    def match_symbol(self, prefix, prefix_regexp, symbol):
        return symbol.startswith(prefix) or symbol.replace("-", "").startswith(prefix) or prefix_regexp.match(symbol)
        
    def search_symbols(self, prefix: str, ticker: int):
        prefix_regexp = re.compile(re.sub(r'([a-zA-Z0-9-_])', r'\1.*', re.escape(prefix)))
        candidates = list(filter(lambda symbol: self.match_symbol(prefix, prefix_regexp, symbol), self.symbols))
                    
        if ticker == self.search_ticker:
            self.message_queue.put({
                "name": "update_backend_items",
                "backend": "elisp_symbols",
                "items": candidates
            })
