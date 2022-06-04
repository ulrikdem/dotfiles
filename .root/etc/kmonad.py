#!/usr/bin/env python3

import re

XX = "XX"
_ = "_"

blocks = [
    ("defcfg", "fallthrough", "true"),
    ("defsrc", *"""
        `    1    2    3    4    5    6    7    8    9    0    -    =    bspc
        tab  q    w    e    r    t         y    u    i    o    p    [    ]    \\
        caps a    s    d    f    g         h    j    k    l    ;    '    ret
        lsft 102d z    x    c    v    b    n    m    ,    .    /    rsft
             lctl lmet lalt           spc            ralt rmet cmps rctl
    """.split()),
]

sparse_layers = set()

def sparse_layer(name, *pairs):
    buttons = [_] * 62
    for i, button in pairs:
        buttons[i] = button
    sparse_layers.add(("deflayer", name, *buttons))

def layer_toggle(layer):
    return ("layer-toggle", layer)
def layer_add(layer):
    return ("layer-add", layer)
def layer_rem(layer):
    return ("layer-rem", layer)
def layer_switch(layer):
    return ("layer-switch", layer)

def around(outer, inner):
    return ("around", outer, inner)
def tap_hold(tap, hold):
    return ("tap-hold-next-release", 200, tap, hold)

def map_button(f, button):
    if type(button) is tuple:
        if button[0] == "around" or button[0].startswith("tap-hold"):
            return (*button[:-2], *(map_button(f, b) for b in button[-2:]))
    elif button != XX and button != _:
        return f(button)
    return button

def wrap_key(button):
    repeat_layer = "repeat-" + {"\\(": "lparen", "\\)": "rparen"}.get(button, button)
    mod = re.fullmatch("[lr](alt|ctl|met|sft)", button)
    if not re.fullmatch(r"[A-Z0-9]|\\_|bspc", button):
        button = around(button, layer_rem("caps"))
    if mod:
        return button
    sparse_layer(repeat_layer, (47, button))
    return around(button, layer_add(repeat_layer))

def layer(name, *buttons):
    assert len(buttons) == 45
    buttons = [map_button(wrap_key, button) for button in buttons]
    blocks.append(("deflayer", name, *(buttons[i] if type(i) is int else i for i in (
        0,  0,  1,  2,  3,  3,  XX, 4,  4,  5,  6,  7,  7,  7,
        8,  9,  10, 11, 12, 13,     14, 15, 16, 17, 18, 19, 19, 19,
        20, 21, 22, 23, 24, 25,     26, 27, 28, 29, 30, 31, _,
        32, 32, 33, 34, 35, 36, _,  37, 38, 39, 40, 41, _,
            44, 44, 42,         43,         42, 44, 44, 44,
    ))))

lmet = "lmet"
lalt = "lalt"
lsft = around("lsft", layer_toggle("shift"))
lctl = "lctl"
rctl = "rctl"
rsft = around("rsft", layer_toggle("shift"))
ralt = "ralt"
rmet = "rmet"

a_met = tap_hold("a", lmet)
r_alt = tap_hold("r", lalt)
s_sft = tap_hold("s", lsft)
t_ctl = tap_hold("t", lctl)
n_ctl = tap_hold("n", rctl)
e_sft = tap_hold("e", rsft)
i_alt = tap_hold("i", lalt)
o_met = tap_hold("o", rmet)
x_alt = tap_hold("x", ralt)
dot_alt = tap_hold(".", ralt)

ret_num = tap_hold("ret", around(layer_toggle("kp"), layer_toggle("num")))
spc_nav = tap_hold("spc", layer_toggle("nav"))
ret_fn = tap_hold("ret", layer_toggle("fn"))

layer("base",
    "`",     "!",     "^",     "$",                                         "\\_",   "-",     "=",     "bspc",
    "tab",   "q",     "w",     "f",     "p",     "b",     "j",     "l",     "u",     "y",     ";",     "\\\\",
    "esc",   a_met,   r_alt,   s_sft,   t_ctl,   "g",     "m",     n_ctl,   e_sft,   i_alt,   o_met,   "'",
             "z",     x_alt,   "c",     "d",     "v",     "k",     "h",     ",",     dot_alt, "/",
                                                 ret_num, spc_nav, ret_fn,
)

layer("shift",
    _,       "@",     "#",     "%",                                         "&",     "*",     _,       _,
    _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,
    _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,
             _,       _,       _,       _,       _,       _,       _,       _,       _,       _,
                                                 _,       _,       _,
)

A_met = tap_hold("A", lmet)
R_alt = tap_hold("R", lalt)
S_sft = tap_hold("S", lsft)
T_ctl = tap_hold("T", lctl)
N_ctl = tap_hold("N", rctl)
E_sft = tap_hold("E", rsft)
I_alt = tap_hold("I", lalt)
O_met = tap_hold("O", rmet)
X_alt = tap_hold("X", ralt)

layer("caps",
    _,       _,       _,       _,                                           _,       _,       _,       _,
    _,       "Q",     "W",     "F",     "P",     "B",     "J",     "L",     "U",     "Y",     _,       _,
    _,       A_met,   R_alt,   S_sft,   T_ctl,   "G",     "M",     N_ctl,   E_sft,   I_alt,   O_met,   _,
             "Z",     X_alt,   "C",     "D",     "V",     "K",     "H",     _,       _,       _,
                                                 _,       _,       _,
)

lt_met = tap_hold("<", lmet)
lp_alt = tap_hold("\\(", lalt)
rp_sft = tap_hold("\\)", lsft)
gt_ctl = tap_hold(">", lctl)
_4_ctl = tap_hold("4", rctl)
_5_sft = tap_hold("5", rsft)
_6_alt = tap_hold("6", lalt)
_0_met = tap_hold("0", rmet)
lb_alt = tap_hold("{", ralt)
_3_alt = tap_hold("3", ralt)

kp = layer_add("kp")

layer("num",
    _,       _,       _,       _,                                           _,       "-",     _,       _,
    _,       XX,      "[",     "]",     XX,      XX,      kp,      "7",     "8",     "9",     ";",     _,
    _,       lt_met,  lp_alt,  rp_sft,  gt_ctl,  XX,      XX,      _4_ctl,  _5_sft,  _6_alt,  _0_met,  _,
             XX,      lb_alt,  "}",     ".",     ",",     XX,      "1",     "2",     _3_alt,  "/",
                                                 "ret",   _,       _,
)

rp_ksft = tap_hold("\\)", around(lsft, layer_toggle("kp-shift")))

layer("kp",
    _,       _,       _,       _,                                           _,       "kp-",   _,       _,
    _,       _,       _,       _,       _,       _,       "nlck",  "kp7",   "kp8",   "kp9",   _,       _,
    _,       _,       _,       rp_ksft, _,       _,       _,       "kp4",   "kp5",   "kp6",   "kp0",   _,
             _,       _,       _,       "kp.",   _,       _,       "kp1",   "kp2",   "kp3",   "kp/",
                                                 "kprt",  _,       _,
)

layer("kp-shift",
    _,       _,       _,       _,                                           _,       "kp*",   "kp+",   _,
    _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,
    _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,       _,
             _,       _,       _,       _,       _,       _,       _,       _,       _,       "?",
                                                 _,       _,       _,
)

layer("fn",
    _,       _,       _,       _,                                           _,       _,       _,       _,
    _,       XX,      XX,      XX,      XX,      XX,      "pause", "f7",    "f8",    "f9",    "f12",   _,
    _,       lmet,    lalt,    lsft,    lctl,    XX,      "slck",  "f4",    "f5",    "f6",    "f11",   _,
             XX,      ralt,    XX,      XX,      XX,      "sys",   "f1",    "f2",    "f3",    "f10",
                                                 _,       _,       "ret",
)

capswrd = around(layer_add("caps"), layer_add("nav"))
qwerty = layer_switch("qwerty")

layer("nav",
    _,       _,       _,       _,                                           _,       _,       _,       _,
    _,       XX,      qwerty,  XX,      XX,      "brup",  "volu",  "home",  "up",    "end",   "pgup",  _,
    _,       lmet,    lalt,    lsft,    lctl,    "brdn",  "vold",  "left",  "down",  "rght",  "pgdn",  _,
             XX,      ralt,    capswrd, XX,      XX,      "mute",  "ins",   "caps",  "cmps",  "del",
                                                 _,       "spc",   _,
)

sparse_layer("qwerty",
    (28, lctl),
    (56, around(lalt, layer_toggle("alt"))),
    (58, around(ralt, layer_toggle("alt"))),
)

sparse_layer("alt",
    (56, layer_switch("base")),
    (58, layer_switch("base")),
)

blocks.extend(sparse_layers)

def to_str(expr):
    if type(expr) is tuple:
        return "(" + " ".join(map(to_str, expr)) + ")"
    return str(expr)

for block in blocks:
    print(to_str(block))
