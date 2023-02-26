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
import shutil

from core.utils import eval_in_emacs, message_emacs, get_project_path    # type: ignore
from core.search import Search    # type: ignore

class SearchFindFile(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        self.sub_process = None
        
    def init_dir(self, search_dir):
        self.search_dir = search_dir
        self.search_path = get_project_path(search_dir)
        
    def search_match(self, prefix):
        prefix = prefix.replace("*", "")
        if len(prefix.split()) > 0:
            if shutil.which("fd"):
                command_list = ["fd", "--regex",".*".join(prefix.split()), "--search-path", self.search_path]
            elif shutil.which("fdfind"):
                command_list = ["fdfind", ".*".join(prefix.split()), "--search-path", self.search_path]
            else:
                return []
            results = self.get_process_result(command_list)        
            
            return list(map(lambda p: os.path.relpath(p, self.search_path), results))
        else:
            return os.listdir(self.search_dir)

    def do(self, candidate):
        eval_in_emacs("blink-search-open-file", os.path.join(self.search_path, candidate))

    def copy(self, candidate):
        path = os.path.join(self.search_path, candidate)
        eval_in_emacs("kill-new", path)
        message_emacs("Copy: {}".format(path))
        
    def parent(self, candidate):
        eval_in_emacs("blink-search-open-file", os.path.dirname(os.path.join(self.search_path, candidate)))

    def continue_search(self, candidate):
        candidate_path = os.path.join(self.search_path, candidate)
        if os.path.isdir(candidate_path):
            continue_path = candidate_path
        else:
            continue_path = os.path.dirname(candidate_path)
        
        eval_in_emacs("blink-search-continue-search", continue_path)

