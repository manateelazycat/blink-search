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
import tempfile
import hashlib
import json

from core.utils import eval_in_emacs, message_emacs, touch    # type: ignore
from core.search import Search    # type: ignore

class SearchCurrentBuffer(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        self.buffer_temp_path = ""
        
    def init_buffer(self, buffer_name, buffer_content):
        import base64
        
        self.buffer_name = buffer_name
        self.buffer_content = base64.b64decode(buffer_content).decode("utf-8")
        
        md5 = hashlib.md5()
        md5.update(buffer_name.encode("utf-8"))
        self.buffer_temp_path = os.path.join(tempfile.gettempdir(), "blink-search-temp-buffer-{}".format(md5.hexdigest()))
        touch(self.buffer_temp_path)
        with open(self.buffer_temp_path, "w", encoding="utf-8") as f:
            f.write(self.buffer_content)
        
    def search_match(self, prefix):
        if os.path.exists(self.buffer_temp_path):
            command_string = "rg -S --json --max-columns 300 '{}' {}".format(prefix, self.buffer_temp_path)
            lines = self.get_process_result(command_string)
            
            results = []
            for line in lines:
                info = json.loads(line)
                if info["type"] == "match":
                    prefix = "{}:{}: ".format(info["data"]["line_number"], info["data"]["submatches"][0]["start"])
                    candidate = "{}{}".format(prefix, info["data"]["lines"]["text"][:-1])
                    matches = list(map(lambda match: [match["start"] + len(prefix) + 1, match["end"] + len(prefix) + 1], info["data"]["submatches"]))
                    results.append({
                        "text": candidate,
                        "matches": matches
                    })
            
            return results
        else:
            return []

    def do(self, candidate):
        candidate_infos = candidate.split(":")
        eval_in_emacs("blink-search-current-buffer-do", 
                      self.buffer_name,
                      int(candidate_infos[0]),
                      int(candidate_infos[1]))
        
        os.unlink(self.buffer_temp_path)

    def select(self, candidate, start_buffer_name):
        candidate_infos = candidate["text"].split(":")
        eval_in_emacs("blink-search-current-buffer-preview", 
                      self.buffer_name,
                      int(candidate_infos[0]),
                      int(candidate_infos[1]))
        
    def copy(self, candidate):
        candidate_text = candidate.split(":")[-1]
        eval_in_emacs("kill-new", candidate_text)
        message_emacs("Copy: {}".format(candidate_text))
