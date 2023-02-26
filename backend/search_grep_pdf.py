import os
import re
import subprocess

from core.utils import eval_in_emacs, get_emacs_var, parse_rg_line
from core.search import Search    # type: ignore

class SearchGrepPDF(Search):

    def __init__(self, backend_name, message_queue) -> None:
        Search.__init__(self, backend_name, message_queue)
        self.sub_process = None
        self.row_number = 100
        self.match_text = None

    def init_dir(self, search_dir):
        self.search_paths = get_emacs_var("blink-search-grep-pdf-search-paths")
        self.search_paths = self.search_paths if self.search_paths else search_dir
        if type(self.search_paths) is str:
            self.search_paths = [self.search_paths]

    def search_items(self, prefix: str, ticker: int):
        prefix = prefix.replace("*", "")
        self.match_text = prefix

        if len(prefix.split()) > 0:

            command_list = ["rga",
                            # Output JSON.
                            "--json",
                            # Smart case.
                            "-S",
                            # Limit column.
                            "--max-columns", "300",
                            # Keyword.
                            ".*".join(prefix.split())]
            for search_path in self.search_paths:
                command_list.append(os.path.expanduser(search_path))

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
        # rga set line_number to None and add Page X: in front of line
        pattern = re.compile('None:.*?: Page ')
        for line in lines:
            if len(self.search_paths) > 1:
                result = parse_rg_line(line, '/')
            else:
                result = parse_rg_line(line, self.search_paths[0])

            if result is not None:
                text = pattern.sub('', result['text'])
                remove_len = len(result['text']) - len(text)
                result['text'] = text

                if len(self.search_paths) > 1:
                    for index, path in enumerate(self.search_paths):
                        if path[1:] in result['text']:
                            text = result['text'].replace(path[1:], f'$D{index}')
                            remove_len += len(result['text']) - len(text)
                            result['text'] = text
                            break

                for match in result['matches']:
                    match[0] -= remove_len
                    match[1] -= remove_len

            if result is not None and \
               '.pdf' in result['text'].split(':')[0]:
                candidates.append(result)

        # save match substring for candidate
        for candidate in candidates:
            text = candidate["text"]

            if len(candidate["matches"]) > 0:
                match = candidate["matches"][0]
                try:
                    candidate['match_text'] = text.encode()[match[0]:match[1]].decode()
                except UnicodeDecodeError:
                    candidate['match_text'] = text[match[0]:match[1]]

                candidate['text'] = text 
            else:
                candidate['match_text'] = prefix

        if ticker == self.search_ticker:
            self.message_queue.put({
                "name": "update_backend_items",
                "backend": self.backend_name,
                "items": candidates,
                "keyword": prefix
            })

    def get_real_path(self, file):
        if len(self.search_paths) == 1:
            return os.path.join(self.search_paths[0], file)
        else:
            for index, path in enumerate(self.search_paths):
                path_mark = f'$D{index}'
                if path_mark in file:
                    return file.replace(path_mark, path)
            return file


    def do(self, candidate):
        candidate_infos = candidate.split(":")
        eval_in_emacs("blink-search-grep-pdf-do",
                      self.get_real_path(candidate_infos[0]),
                      int(candidate_infos[1]),
                      self.match_text)

    def select(self, candidate, start_buffer_name):
        candidate_infos = candidate["text"].split(":")
        self.match_text = candidate['match_text']
        eval_in_emacs("blink-search-grep-pdf-preview",
                      self.get_real_path(candidate_infos[0]),
                      int(candidate_infos[1]),
                      candidate['match_text'])

    def clean(self):
        self.kill_sub_process()
        eval_in_emacs("blink-search-grep-pdf-clean")
