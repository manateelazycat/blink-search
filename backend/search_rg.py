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
import signal

from core.utils import get_command_result, eval_in_emacs    # type: ignore
from core.search import Search    # type: ignore

class SearchRg(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        self.sub_process = None
        
    def init_dir(self, search_dir):
        self.search_dir = search_dir
        self.search_path = self.search_dir
        git_project_path = get_command_result("git rev-parse --show-toplevel", self.search_dir)
        if os.path.exists(git_project_path):
            self.search_path = git_project_path
        
    def search_match(self, prefix):
        if len(prefix.split()) > 0:
            if self.sub_process != None:
                try:
                    os.killpg(os.getpgid(self.sub_process.pid), signal.SIGTERM)
                except:
                    pass
                
            command_string = "rg -S --no-heading --column --max-columns 300 '{}' {}".format(prefix, self.search_dir)
            self.sub_process = subprocess.Popen(command_string, cwd=None, shell=True, text=True,
                                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                                 encoding="utf-8")
            ret = self.sub_process.wait()
            results = []
            if ret == 0:
                results = list(map(lambda p: p.strip(), self.sub_process.stdout.readlines()))    # type: ignore
            
            return list(map(lambda p: os.path.relpath(p, self.search_path), results))
        else:
            return []

    def do(self, candidate):
        candidate_infos = candidate.split(":")
        eval_in_emacs("blink-search-jump-to-file", 
                      os.path.join(self.search_path, candidate_infos[0]),
                      int(candidate_infos[1]),
                      int(candidate_infos[2]) - 1)
