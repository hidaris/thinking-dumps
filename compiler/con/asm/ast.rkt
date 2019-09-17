#lang racket

;; type Asm = [Listof Instruction]

;; type Instruction =
;; | `ret
;; | `(mov ,Arg ,Arg)
;; | `(add ,Arg ,Arg)
;; | `(sub ,Arg ,Arg)
;; | `(cmp ,Arg ,Arg)
;; | `(jmp ,Label)
;; | `(je  ,Label)
;; | `(jne ,Label)
;; | Label

;; type Label = Symbol

;; type Arg =
;; | Reg
;; | Integer

;; type Reg =
;; | `rax
