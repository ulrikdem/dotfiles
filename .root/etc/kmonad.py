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
    assert len(buttons) == 44
    buttons = [map_button(wrap_key, button) for button in buttons]
    blocks.append(("deflayer", name, *(buttons[i] if type(i) is int else i for i in (
        0,  0,  1,  2,  3,  3,  XX, 4,  4,  5,  6,  7,  7,  7,
        8,  9,  10, 11, 12, 13,     14, 15, 16, 17, 18, 19, 19, 19,
        20, 21, 22, 23, 24, 25,     26, 27, 28, 29, 30, 31, _,
        32, 32, 33, 34, 35, 36, _,  37, 38, 39, 40, 41, _,
            _,  42, 42,         43,         42, 42, 42, _,
    ))))

lmet = "lmet"
lalt = "lalt"
lsft = around("lsft", layer_toggle("shift"))
lctl = "lctl"
rctl = "rctl"
rsft = around("rsft", layer_toggle("shift"))
ralt = "ralt"
rmet = "rmet"

mt_a = tap_hold("a", lmet)
mt_r = tap_hold("r", lalt)
mt_s = tap_hold("s", lsft)
mt_t = tap_hold("t", lctl)
mt_n = tap_hold("n", rctl)
mt_e = tap_hold("e", rsft)
mt_i = tap_hold("i", lalt)
mt_o = tap_hold("o", rmet)
mt_x = tap_hold("x", ralt)
mt_dot = tap_hold(".", ralt)

lt_ret = tap_hold("ret", around(around(layer_toggle("kp"), layer_toggle("fn")), layer_toggle("num")))
lt_spc = tap_hold("spc", layer_toggle("nav"))

layer("base",
    "`",    "!",    "^",    "$",                                    "\\_",  "-",    "=",    "bspc",
    "tab",  "q",    "w",    "f",    "p",    "b",    "j",    "l",    "u",    "y",    ";",    "\\\\",
    "esc",  mt_a,   mt_r,   mt_s,   mt_t,   "g",    "m",    mt_n,   mt_e,   mt_i,   mt_o,   "'",
            "z",    mt_x,   "c",    "d",    "v",    "k",    "h",    ",",    mt_dot, "/",
                                            lt_ret, lt_spc,
)

layer("shift",
    _,      "@",    "#",    "%",                                    "&",    "*",    _,      _,
    _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
    _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
            _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
                                            _,      _,
)

mt_A = tap_hold("A", lmet)
mt_R = tap_hold("R", lalt)
mt_S = tap_hold("S", lsft)
mt_T = tap_hold("T", lctl)
mt_N = tap_hold("N", rctl)
mt_E = tap_hold("E", rsft)
mt_I = tap_hold("I", lalt)
mt_O = tap_hold("O", rmet)
mt_X = tap_hold("X", ralt)

layer("caps",
    _,      _,      _,      _,                                      _,      _,      _,      _,
    _,      "Q",    "W",    "F",    "P",    "B",    "J",    "L",    "U",    "Y",    _,      _,
    _,      mt_A,   mt_R,   mt_S,   mt_T,   "G",    "M",    mt_N,   mt_E,   mt_I,   mt_O,   _,
            "Z",    mt_X,   "C",    "D",    "V",    "K",    "H",    _,      _,      _,
                                            _,      _,
)

mt_lt = tap_hold("<", lmet)
mt_lp = tap_hold("\\(", lalt)
mt_rp = tap_hold("\\)", lsft)
mt_gt = tap_hold(">", lctl)
mt_4 = tap_hold("4", rctl)
mt_5 = tap_hold("5", rsft)
mt_6 = tap_hold("6", lalt)
mt_0 = tap_hold("0", rmet)
mt_lb = tap_hold("{", ralt)
mt_3 = tap_hold("3", ralt)

kp = layer_add("kp")
fn = layer_add("fn")

layer("num",
    _,      _,      _,      _,                                      _,      "-",    _,      _,
    _,      XX,     "[",    "]",    XX,     XX,     kp,     "7",    "8",    "9",    ";",    _,
    _,      mt_lt,  mt_lp,  mt_rp,  mt_gt,  XX,     fn,     mt_4,   mt_5,   mt_6,   mt_0,   _,
            XX,     mt_lb,  "}",    ".",    ",",    XX,     "1",    "2",    mt_3,   "/",
                                            "ret",  _,
)

kp_rp = tap_hold("\\)", around(lsft, layer_toggle("kp-shift")))

layer("kp",
    _,      _,      _,      _,                                      _,      "kp-",  _,      _,
    _,      _,      _,      _,      _,      _,      "nlck", "kp7",  "kp8",  "kp9",  _,      _,
    _,      _,      _,      kp_rp,  _,      _,      XX,     "kp4",  "kp5",  "kp6",  "kp0",  _,
            _,      _,      _,      "kp.",  _,      _,      "kp1",  "kp2",  "kp3",  "kp/",
                                            "kprt", _,
)

layer("kp-shift",
    _,      _,      _,      _,                                      _,      "kp*",  "kp+",  _,
    _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
    _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
            _,      _,      _,      _,      _,      _,      _,      _,      _,      "/",
                                            _,      _,
)

layer("fn",
    _,      _,      _,      _,                                      _,      _,      _,      _,
    _,      _,      _,      _,      _,      _,      "pause","f7",   "f8",   "f9",   "f12",  _,
    _,      _,      _,      _,      _,      _,      "slck", "f4",   "f5",   "f6",   "f11",  _,
            _,      _,      _,      _,      _,      "sys",  "f1",   "f2",   "f3",   "f10",
                                            _,      _,
)

capwrd = around(layer_add("caps"), layer_add("nav"))
qwerty = layer_switch("qwerty")

layer("nav",
    _,      _,      _,      _,                                      _,      _,      _,      _,
    _,      XX,     qwerty, XX,     XX,     "brup", "volu", "home", "up",   "end",  "pgup", _,
    _,      lmet,   lalt,   lsft,   lctl,   "brdn", "vold", "left", "down", "rght", "pgdn", _,
            XX,     ralt,   capwrd, XX,     XX,     "mute", "ins",  "caps", "cmps", "del",
                                            _,      "spc",
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
