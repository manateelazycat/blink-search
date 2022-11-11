#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2022 EdmondFrank
#
# Author:     EdmondFrank <edmomdfrank@yahoo.com>
# Maintainer: <edmomdfrank@yahoo.com>
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

try:
    import sqlite3
except ImportError:
    print("can not import sqlite3, please run `pip install sqlite3` at first")


from core.utils import get_emacs_var, eval_in_emacs, message_emacs
from core.search import Search    # type: ignore


class SearchKeyValueStore(Search):
    """
    Key Value Store Backend for blink-search
    """
    def __init__(self, backend_name, message_queue) -> None:
        blink_search_db_path = get_emacs_var("blink-search-db-path")
        blink_search_db_table = get_emacs_var("blink-search-db-table")
        self.table = blink_search_db_table
        self.conn = self.create_connection(blink_search_db_path)

        init = f"CREATE TABLE IF NOT EXISTS {self.table} (key TEXT NOT NULL, value TEXT NOT NULL);"
        if self.conn:
            self.conn.cursor().execute(init)
        self.set_regexp = re.compile(r"^set\s([^\s]+)\s([^\s]+)$")
        self.del_regexp = re.compile(r"^del\s([^\s]+)$")

        Search.__init__(self, backend_name, message_queue)

    @staticmethod
    def create_connection(db_file):
        """
        create a database connection to the SQLite databasespecified by db_file
        :param db_file: database file
        :return: Connection object or None
        """
        conn = None
        try:
            conn = sqlite3.connect(db_file, check_same_thread=False)
        except Exception as err:  # pylint: disable=broad-except
            print(err)

        return conn

    def search_match(self, prefix):
        if not self.conn:
            return []

        query = f"SELECT key FROM {self.table} WHERE key LIKE '%{prefix}%' LIMIT 20;"
        candidates = list(map(lambda row: row[0], self.conn.cursor().execute(query).fetchall()))
        if self.set_regexp.match(prefix) or self.del_regexp.match(prefix):
            candidates.append(prefix)

        return candidates

    def do(self, candidate):
        if not self.conn:
            message_emacs("Invalid connection to sqlite3")

        set_matches = self.set_regexp.match(candidate)
        del_matches = self.del_regexp.match(candidate)
        if set_matches:
            key = set_matches.group(1)
            value = set_matches.group(2)
            exists = f"SELECT 1 FROM {self.table} WHERE key = '{key}';"
            if self.conn.cursor().execute(exists).fetchone():
                # Update existed iteam
                update = f"UPDATE {self.table} SET value = '{value}' WHERE key = '{key}';"
                self.conn.cursor().execute(update)
                self.conn.commit()
                message_emacs(f"Updated key-value ({key}, {value}) successfully")
            else:
                # Insert new iteam
                insert = f"INSERT INTO {self.table} (key, value) VALUES (?, ?);"
                self.conn.cursor().execute(insert, (key, value))
                self.conn.commit()
                message_emacs(f"Inserted key-value ({key}, {value}) successfully")
        elif del_matches:
            key = del_matches.group(1)
            delete = f"DELETE FROM {self.table} WHERE key = '{key}';"
            self.conn.cursor().execute(delete)
            self.conn.commit()
            message_emacs(f"Deleted {key} successfully")
        else:
            query = f"SELECT value FROM {self.table} WHERE key = '{candidate}';"
            value = self.conn.cursor().execute(query).fetchone()[0]
            eval_in_emacs("kill-new", value)

    def parent(self, candidate):
        self.do(candidate)
