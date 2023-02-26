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
import subprocess

from core.utils import eval_in_emacs, message_emacs, get_project_path, parse_rg_line
from core.search import Search    # type: ignore

class SearchGrepFile(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        self.sub_process = None
        self.row_number = 100
        
    def init_dir(self, search_dir):
        self.search_path = get_project_path(search_dir)
        
    def search_items(self, prefix: str, ticker: int):
        prefix = prefix.replace("*", "")
        if len(prefix.split()) > 0:
            
            command_list = ["rg", 
                            # Output JSON.
                            "--json",               
                            # Smart case.
                            "-S",                   
                            # Limit column.
                            "--max-columns", "300", 
                            # Ignore unnecessary files.
                            "-g", "!node_modules",  
                            "-g", "!__pycache__",  
                            "-g", "!dist",  
                            # Keyword.
                            ".*".join(prefix.split()), 
                            # Search directory.
                            os.path.expanduser(self.search_path)
                            ]
            
            self.kill_sub_process()
                
            self.sub_process = subprocess.Popen(command_list, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            
            lines = []
            try:
                while True:
                    if self.sub_process is None:
                        break
                    
                    line = self.sub_process.stdout.readline()    # type: ignore
                    
                    if not line: 
                        break
                    
                    lines.append(line.strip())
                    
                    if len(lines) == self.row_number:
                        self.parse_lines(lines, prefix, ticker)
            except:
                import traceback
                traceback.print_exc()
            finally:
                self.kill_sub_process()
                
            self.parse_lines(lines, prefix, ticker)
            
    def parse_lines(self, lines, prefix, ticker):
        candidates = []
        for line in lines:
            result = parse_rg_line(line, self.search_path)
            if result is not None:
                candidates.append(result)
            
        if ticker == self.search_ticker:
            self.message_queue.put({
                "name": "update_backend_items",
                "backend": self.backend_name,
                "items": candidates,
                "keyword": prefix
            })
            
    def do(self, candidate):
        candidate_infos = candidate.split(":")
        eval_in_emacs("blink-search-grep-file-do", 
                      os.path.join(self.search_path, candidate_infos[0]),
                      int(candidate_infos[1]),
                      int(candidate_infos[2]))

    def select(self, candidate, start_buffer_name):
        candidate_infos = candidate["text"].split(":")
        eval_in_emacs("blink-search-grep-file-preview", 
                      os.path.join(self.search_path, candidate_infos[0]),
                      int(candidate_infos[1]),
                      int(candidate_infos[2]))

    def copy(self, candidate):
        candidate_text = candidate.split(":")[-1]
        eval_in_emacs("kill-new", candidate_text)
        message_emacs("Copy: {}".format(candidate_text))
        

    def parent(self, candidate):
        candidate_infos = candidate.split(":")
        eval_in_emacs("blink-search-open-file", os.path.dirname(os.path.join(self.search_path, candidate_infos[0])))

        
    def clean(self):
        self.kill_sub_process()
        eval_in_emacs("blink-search-grep-file-clean")
