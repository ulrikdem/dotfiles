#!/usr/bin/env python3

import re
from collections import namedtuple

blocks = [
    ["defcfg", "fallthrough", "true"],
    ["defsrc", *"""
        `    1    2    3    4    5    6    7    8    9    0    -    =    bspc
        tab  q    w    e    r    t         y    u    i    o    p    [    ]    \\
        caps a    s    d    f    g         h    j    k    l    ;    '    ret
        lsft 102d z    x    c    v    b    n    m    ,    .    /    rsft
             lctl lmet lalt           spc            ralt rmet cmps rctl
    """.split()],
]

XX = "XX"
_ = "_"

around = namedtuple("around", ("outer", "inner"))
layer_add = namedtuple("layer_add", ("layer"))
layer_switch = namedtuple("layer_switch", ("layer"))
layer_toggle = namedtuple("layer_toggle", ("layer"))
tap_hold_next_release = namedtuple("tap_hold_next_release", ("delay", "tap", "hold"))

def tap_hold(tap, hold):
    return tap_hold_next_release(200, tap, hold)

def map_button(f, button):
    if isinstance(button, tuple):
        if isinstance(button, around):
            return around(map_button(f, button.outer), map_button(f, button.inner))
        elif isinstance(button, tap_hold_next_release):
            return button._replace(tap=map_button(f, button.tap), hold=map_button(f, button.hold))
    elif button != XX and button != _:
        return f(button)
    return button

repeat_layers = {}

def add_repeat(key):
    if re.fullmatch("[lr](alt|ctl|met|sft)", key):
        return key
    layer = "repeat-" + {"\\(": "lparen", "\\)": "rparen"}.get(key, key)
    repeat_layers[layer] = ["deflayer", layer] + [_] * 47 + [key] + [_] * 14
    return around(key, layer_add(layer))

def layer(name, *buttons):
    buttons = [map_button(add_repeat, button) for button in buttons]
    blocks.append(["deflayer", name, *(buttons[i] if isinstance(i, int) else i for i in [
        0,  0,  1,  2,  3,  3,  XX, 4,  4,  5,  6,  7,  7,  7,
        8,  9,  10, 11, 12, 13,     14, 15, 16, 17, 18, 19, 19, 19,
        20, 21, 22, 23, 24, 25,     26, 27, 28, 29, 30, 31, _,
        32, 32, 33, 34, 35, 36, _,  37, 38, 39, 40, 41, _,
            _,  42, 42,         43,         42, 42, 42, _,
    ])])

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
    _,      "2",    "3",    "5",                                    "7",    "8",    _,      _,
    _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
    _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
            _,      _,      _,      _,      _,      _,      _,      _,      _,      _,
                                            _,      _,
)

add_kp = layer_add("kp")
add_fn = layer_add("fn")

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

layer("num",
    _,      _,      _,      _,                                      _,      "-",    _,      _,
    _,      XX,     "[",    "]",    XX,     XX,     add_kp, "7",    "8",    "9",    ";",    _,
    _,      mt_lt,  mt_lp,  mt_rp,  mt_gt,  XX,     add_fn, mt_4,   mt_5,   mt_6,   mt_0,   _,
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

qwerty = layer_switch("qwerty")

layer("nav",
    _,      _,      _,      _,                                      _,      _,      _,      _,
    _,      XX,     qwerty, XX,     XX,     "brup", "volu", "home", "up",   "end",  "pgup", _,
    _,      lmet,   lalt,   lsft,   lctl,   "brdn", "vold", "left", "down", "rght", "pgdn", _,
            XX,     ralt,   XX,     XX,     XX,     "mute", "ins",  "caps", "cmps", "del",
                                            _,      "spc",
)

buttons = [_] * 62
buttons[56] = buttons[58] = layer_switch("base")
blocks.append(["deflayer", "alt", *buttons])
buttons[56] = around(lalt, layer_toggle("alt"))
buttons[58] = around(ralt, layer_toggle("alt"))
buttons[28] = lctl
blocks.append(["deflayer", "qwerty", *buttons])

blocks.extend(repeat_layers.values())

def to_str(expr):
    if isinstance(expr, tuple):
        expr = [type(expr).__name__.replace("_", "-"), *expr]
    if isinstance(expr, list):
        return "(" + " ".join(map(to_str, expr)) + ")"
    return str(expr)

for block in blocks:
    print(to_str(block))
