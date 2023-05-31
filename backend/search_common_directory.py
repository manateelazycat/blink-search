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

import re
import os

from core.search import Search
from core.utils import eval_in_emacs, get_emacs_var    # type: ignore


class SearchCommonDirectory(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        
    def search_match(self, prefix):
        prefix = prefix.replace("*", "")
        prefix_regexp = re.compile(".*" + ".*".join(prefix.split()))
        
        common_directory = get_emacs_var("blink-search-common-directory")

        if type(common_directory) is list and len(common_directory) > 0:
            results = []
            
            for directory_info in common_directory:
                alias = directory_info[0]
                directory = os.path.expanduser(directory_info[1])
                
                for path in os.listdir(directory):
                    if self.is_match(prefix, prefix_regexp, "{} {}".format(alias.lower(), path.lower())):
                        results.append("{} {}".format(alias, path))
                            
            return sorted(results)
        else:
            return []

    def get_candiate_dir(self, candidate):
        prefix = candidate.split()[0]
        common_directory = get_emacs_var("blink-search-common-directory")
        if type(common_directory) is list:
            for directory_info in common_directory:
                alias = directory_info[0]
                directory = os.path.expanduser(directory_info[1])
                
                if prefix == alias:
                    return os.path.join(directory, candidate.replace(prefix, "").strip())
        
        return None
        
    def do(self, candidate):
        candidate_dir = self.get_candiate_dir(candidate)
        if candidate_dir is not None:
            eval_in_emacs("blink-search-open-file", candidate_dir)

    def parent(self, candidate):
        candidate_dir = self.get_candiate_dir(candidate)
        if candidate_dir is not None:
            eval_in_emacs("blink-search-open-file", os.path.dirname(candidate_dir))

    def continue_search(self, candidate):
        candidate_dir = self.get_candiate_dir(candidate)
        if candidate_dir is not None:
            eval_in_emacs("blink-search-continue-search", candidate_dir)
            
