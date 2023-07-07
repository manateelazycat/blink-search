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
import re

from core.utils import get_emacs_var
from core.search import Search


class SearchHistory(Search):
    
    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)

        self.dispatch_candiate_callback = None
        
    def search_match(self, prefix):
        try:
            prefix = prefix.replace("*", "")

            history_path = get_emacs_var("blink-search-history-path")
            if os.path.exists(history_path):
                histories = []
                with open(history_path, encoding="utf-8", errors="ignore") as f:
                    histories = f.read().splitlines()

                match_histories = []
                prefix_regexp = re.compile(".*" + ".*".join(prefix.split()))
                for history in histories:
                    history_infos = history.split("ᛡ")
                    if len(history_infos) >= 2:
                        if prefix_regexp.match(history_infos[0].lower()) or prefix_regexp.match(history_infos[1].lower()):
                            match_histories.append(f"{history_infos[0]} [{history_infos[1]}]")

                return match_histories
        except:
            import traceback
            traceback.print_exc()
            return []

    def do(self, candidate):
        pattern = r'^(.+)\s+\[([^\[\]]+)\]$'

        match = re.search(pattern, candidate)

        if self.dispatch_candiate_callback and match:
            match_candidate = match.group(1)
            match_backend = match.group(2)

            self.dispatch_candiate_callback(match_backend, match_candidate)
