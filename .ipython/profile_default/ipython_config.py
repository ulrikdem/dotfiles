import os

c.TerminalIPythonApp.display_banner = False
c.TerminalInteractiveShell.confirm_exit = False

if 'DISPLAY' in os.environ:
    c.InteractiveShellApp.pylab = 'tk'
    c.InteractiveShellApp.pylab_import_all = False
else:
    c.InteractiveShellApp.exec_lines = ['import numpy as np']
