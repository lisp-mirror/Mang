(load "./mang.asd")

(ql:quickload "mang")

(save-lisp-and-die "mang"
                   :toplevel #'mang::mang
                   :executable t
                   :purify t
                   :compression 9)
