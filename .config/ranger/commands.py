from os.path import abspath
from ranger.api.commands import Command
from subprocess import PIPE

class fzf_select(Command):
    def execute(self):
        p = self.fm.execute_command('fd -L0 --strip-cwd-prefix | fzf --read0 --reverse --prompt="${PWD%/}/"', stdout=PIPE)
        stdout = p.communicate()[0]
        if p.returncode == 0:
            self.fm.select_file(abspath(stdout.decode()[:-1]))
