#!/usr/bin/sbcl --script

;; listの長さを返す関数
(defun my_len (list)
  (if (equal list ())
    0
    (+ 1 (my_len(cdr list)))))

;; lの要素の後半にrを追加する関数
(defun my-append (l r)
  (if (equal l '())
      r
      (cons (car l) (my-append (cdr l) r))))

;; 文字のlistを数値に変換する関数
;; example
; (print (digit_list_to_num '(#\4 #\2) 2)) -> 42
(defun digit_list_to_num (dl cur_len)
  (if (equal (cdr dl) ())
      (digit-char-p (car dl))
      (+ (* (expt 10 (- cur_len 1)) (digit-char-p (car dl))) (digit_list_to_num (cdr dl) (- cur_len 1)))))

;; listの要素を逆順にする関数
;; example
; (print (my_reverse '(5 4 3 2 1))) -> (1 2 3 4 5)
(defun my_reverse(l)
  (if (equal (cdr l) '())
      (cons (car l) nil)
      (append (my_reverse (cdr l)) (cons (car l) nil))))

;; reverseしてあること前提の関数. こっちはcur_lenに0を渡せるので、事前に数値の長さを知ってる必要がない
;; example
;; (print (digit_list_to_num2 (my_reverse '(#\2 #\4)) 0)) -> 42
(defun digit_list_to_num2 (dl cur_len)
  (if (equal (cdr dl) '())
      (* (expt 10 cur_len) (digit-char-p (car dl)))
      (+ (* (expt 10 cur_len) (digit-char-p (car dl))) (digit_list_to_num2 (cdr dl) (+ cur_len 1)))))

;; 1-line読んで、space区切りで数値のlistにして返す関数. 競プロとかで使えそう
;; example
;; (print to_list(read-line)) ( "123 456" -> (123 456) )
(defun to_list(line)
  (let ((tmp '())
        (arr '()))
        (loop :for char :across line :do
          (princ char)
          (if (equal char #\ )
          (setq arr (append arr (cons (digit_list_to_num2 (my_reverse tmp) 0) nil)) tmp nil)
          (setq tmp (append tmp (cons char nil)))))
        (setq arr (append arr (cons (digit_list_to_num2 (my_reverse tmp) 0) nil)))
        arr))
