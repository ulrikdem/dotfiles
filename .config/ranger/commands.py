from os.path import abspath
from ranger.api.commands import Command
from ranger.gui import color
from subprocess import PIPE

color.BRIGHT = 0

class fzf_cd(Command):
    def execute(self):
        p = self.fm.execute_command('fd -L0td | fzf --read0 --reverse --prompt="$PWD/"', stdout=PIPE)
        stdout = p.communicate()[0]
        if p.returncode == 0:
            self.fm.cd(abspath(stdout.decode()[:-1]))

class fzf_select(Command):
    def execute(self):
        p = self.fm.execute_command('fd -L0 | fzf --read0 --reverse --prompt="$PWD/"', stdout=PIPE)
        stdout = p.communicate()[0]
        if p.returncode == 0:
            self.fm.select_file(abspath(stdout.decode()[:-1]))
