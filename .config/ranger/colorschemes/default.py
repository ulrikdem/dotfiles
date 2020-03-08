from ranger.colorschemes.default import Default
from ranger.gui import color

class Scheme(Default):
    def use(self, context):
        fg, bg, attr = super().use(context)

        if fg >= color.BRIGHT:
            fg -= color.BRIGHT

        if context.in_browser:
            fg, bg, attr = color.default_colors

            if context.copied or context.cut:
                fg = color.black + color.BRIGHT
                attr |= color.bold
            elif context.marked and context.main_column:
                fg = color.yellow
                attr |= color.bold
            elif context.link:
                fg = color.cyan if context.good else color.red
            elif context.directory:
                fg = color.blue
            elif context.fifo or context.socket:
                fg = color.magenta
            elif context.device:
                fg = color.yellow
            elif context.executable:
                fg = color.green

            if context.selected:
                attr |= color.reverse
            elif context.tag_marker:
                fg = color.red

            if context.empty or context.error:
                fg = color.red
                attr |= color.reverse

            if context.inactive_pane:
                fg = color.black + color.BRIGHT

        if context.tab:
            if context.good:
                bg = color.default
            else:
                fg = color.black + color.BRIGHT
                attr &= ~color.bold

        return fg, bg, attr
