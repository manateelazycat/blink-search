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
import requests
import json

from core.utils import eval_in_emacs    # type: ignore
from core.search import Search    # type: ignore

class SearchGoogleSuggest(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        
    def is_valid_url(self, url: str):
        if len(url.split()) > 1:
            return False
        
        if url.startswith('http://') or url.startswith('ftp://') or url.endswith(".html"):
            return True
        
        import re
        pattern = re.compile(
            r'^(([a-zA-Z]{1})|([a-zA-Z]{1}[a-zA-Z]{1})|'
            r'([a-zA-Z]{1}[0-9]{1})|([0-9]{1}[a-zA-Z]{1})|'
            r'([a-zA-Z0-9][-_.a-zA-Z0-9]{0,61}[a-zA-Z0-9]))\.'
            r'([a-zA-Z]{2,13}|[a-zA-Z0-9-]{2,30}.[a-zA-Z]{2,3})$'
        )
        
        return pattern.match(url)
        
    def search_match(self, prefix):
        if len(prefix.split()) > 0:
            query = prefix.replace(" ", "%20")
            response = requests.get('http://google.com/complete/search?client=chrome&q={}'.format(query), headers={
                "User-Agent":
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36 Edge/18.19582"
            })
            
            if self.is_valid_url(prefix) and (not (prefix.startswith("http://") or prefix.startswith("ftp://"))):
                prefix = "http://" + prefix
                
            return [prefix] + json.loads(response.text)[1]
        else:
            return []

    def do(self, candidate):
        if self.is_valid_url(candidate):
            eval_in_emacs("blink-search-browser-function", candidate)
        else:
            eval_in_emacs("blink-search-browser-function", "http://www.google.com/search?q={}".format(candidate.replace(" ", "%20")))
