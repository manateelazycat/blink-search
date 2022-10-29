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
from core.search import Search    # type: ignore


class SearchBufferList(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        
    def is_match(self, prefix, prefix_regexp, symbol):
        return symbol.startswith(prefix) or symbol.replace("-", "").startswith(prefix) or prefix_regexp.match(symbol)
    
    def search_match(self, prefix):
        if len(prefix.split()) > 0:
            prefix_regexp = re.compile(".*" + ".*".join(prefix.split()))
            return list(filter(lambda symbol: self.is_match(prefix, prefix_regexp, symbol), self.items))
        else:
            return []
    
