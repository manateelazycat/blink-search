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

import os

from core.utils import eval_in_emacs
from core.search import Search    # type: ignore


class SearchRecentFile(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        
    def do(self, candidate):
        eval_in_emacs("find-file", candidate)

    def parent(self, candidate):
        eval_in_emacs("blink-search-open-file", os.path.dirname(candidate))

