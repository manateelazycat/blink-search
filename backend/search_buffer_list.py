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

import functools
import re

from core.utils import eval_in_emacs
from core.search import Search    # type: ignore


class SearchBufferList(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        
    def sort_buffer(self, a, b):
        if (not a.startswith(" *")) and b.startswith(" *"):
            return -1
        elif a.startswith(" *") and (not b.startswith(" *")):
            return 1
        else:
            return a < b
        
    def update(self, items):
        self.items = sorted(items, key=functools.cmp_to_key(self.sort_buffer))
        
    def update_sort_buffers(self, items):
        self.items = list(dict.fromkeys(items))
        
    def search_match(self, prefix: str):
        prefix_regexp = re.compile(".*" + ".*".join(prefix.replace("*", "").split()), re.IGNORECASE)
        match_items = list(filter(lambda symbol: self.is_match(prefix, prefix_regexp, symbol), self.items))
        
        if prefix.startswith("*"):
            return list(filter(lambda i: i.startswith("*"), match_items))
        elif prefix.startswith(" *"):
            return list(filter(lambda i: i.startswith(" *"), match_items))
        elif prefix.endswith("*"):
            return list(filter(lambda i: i.endswith("*"), match_items))
        else:
            return match_items
        
    def do(self, candidate):
        eval_in_emacs("switch-to-buffer", candidate)

