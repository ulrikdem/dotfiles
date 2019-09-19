from alot.settings.const import settings
from alot.settings.manager import SettingsManager
from asyncio import create_subprocess_exec
from asyncio.subprocess import DEVNULL
from email.utils import make_msgid

def get_theming_attribute(self, mode, name, part=None):
    remap = {
        ('global', 'notify_normal'): ('global', 'prompt'),
        ('global', 'notify_error'): ('thread', 'arrow_heads'),
    }
    mode, name = remap.get((mode, name), (mode, name))
    return self.get_theming_attribute_old(mode, name, part)

SettingsManager.get_theming_attribute_old = SettingsManager.get_theming_attribute
SettingsManager.get_theming_attribute = get_theming_attribute

def store_focus(buf):
    if buf.modename == 'search':
        thread = buf.get_selected_thread()
        buf.tid = thread and thread.get_thread_id()
        return True
    return False

def restore_focus(buf):
    if buf.modename == 'search' and buf.tid:
        for i, line in enumerate(buf.threadlist.get_lines()):
            if line.get_thread().get_thread_id() == buf.tid:
                buf.body.set_focus(i)
                break

def pre_buffer_focus(ui, dbm, buf):
    if store_focus(buf):
        buf.rebuild()

def post_buffer_focus(ui, dbm, buf, success):
    restore_focus(buf)

async def pre_search_refresh(ui, dbm, cmd):
    msg = ui.notify('syncing...', timeout=-1)
    proc = await create_subprocess_exec(
        'gmi', 'sync',
        cwd=dbm.path,
        stdin=DEVNULL,
        stdout=DEVNULL,
        stderr=DEVNULL,
    )
    status = await proc.wait()
    ui.clear_notify([msg])
    if status:
        ui.notify('sync failed', priority='error')
    store_focus(ui.current_buffer)

async def post_search_refresh(ui, dbm, cmd):
    restore_focus(ui.current_buffer)
    ui.update()

async def pre_envelope_send(ui, dbm, cmd):
    ui.current_buffer.envelope.headers['Message-ID'] = [make_msgid(domain='localhost')]
