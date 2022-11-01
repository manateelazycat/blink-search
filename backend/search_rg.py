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

from core.utils import get_command_result, eval_in_emacs, get_project_path    # type: ignore
from core.search import Search    # type: ignore

class SearchRg(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        self.sub_process = None
        
    def init_dir(self, search_dir):
        self.search_path = get_project_path(search_dir)
        
    def search_match(self, prefix):
        prefix = prefix.replace("*", "")
        if len(prefix.split()) > 0:
            command_string = "rg -S --no-heading --column --max-columns 300 '{}' {}".format(".*".join(prefix.split()), self.search_path)
            results = self.get_process_result(command_string)
            
            return list(map(lambda p: os.path.relpath(p, self.search_path), results))
        else:
            return []

    def do(self, candidate):
        candidate_infos = candidate.split(":")
        eval_in_emacs("blink-search-rg-do", 
                      os.path.join(self.search_path, candidate_infos[0]),
                      int(candidate_infos[1]),
                      int(candidate_infos[2]) - 1)
