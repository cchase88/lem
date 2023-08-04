(defpackage :lem/html-renderer
  (:use :cl :lem))
(in-package :lem/html-renderer)

(defvar *h1-font* (sdl2-ttf:open-font "/Users/cxxxr/common-lisp/lem/frontends/sdl2/resources/fonts/NotoSansMono-Bold.ttf" 80))
(defvar *h2-font* (sdl2-ttf:open-font "/Users/cxxxr/common-lisp/lem/frontends/sdl2/resources/fonts/NotoSansMono-Bold.ttf" 64))
(defvar *h3-font* (sdl2-ttf:open-font "/Users/cxxxr/common-lisp/lem/frontends/sdl2/resources/fonts/NotoSansMono-Bold.ttf" 50))
(defvar *h4-font* (sdl2-ttf:open-font "/Users/cxxxr/common-lisp/lem/frontends/sdl2/resources/fonts/NotoSansMono-Bold.ttf" 40))
(defvar *h5-font* (sdl2-ttf:open-font "/Users/cxxxr/common-lisp/lem/frontends/sdl2/resources/fonts/NotoSansMono-Bold.ttf" 32))
(defvar *h6-font* (sdl2-ttf:open-font "/Users/cxxxr/common-lisp/lem/frontends/sdl2/resources/fonts/NotoSansMono-Bold.ttf" 26))

(defvar *h1-attribute* (make-attribute :plist (list 'lem-sdl2::font *h1-font*)))
(defvar *h2-attribute* (make-attribute :plist (list 'lem-sdl2::font *h2-font*)))
(defvar *h3-attribute* (make-attribute :plist (list 'lem-sdl2::font *h3-font*)))
(defvar *h4-attribute* (make-attribute :plist (list 'lem-sdl2::font *h4-font*)))
(defvar *h5-attribute* (make-attribute :plist (list 'lem-sdl2::font *h5-font*)))
(defvar *h6-attribute* (make-attribute :plist (list 'lem-sdl2::font *h6-font*)))

(defvar *point*)

(defgeneric traverse-aux (tag body))

(defmethod traverse-aux ((tag (eql :p)) body)
  (traverse (second body)))

(defmethod traverse-aux ((tag (eql :h1)) body)
  (insert-string *point* (second body) :attribute *h1-attribute*))

(defmethod traverse-aux ((tag (eql :h2)) body)
  (insert-string *point* (second body) :attribute *h2-attribute*))

(defmethod traverse-aux ((tag (eql :h3)) body)
  (insert-string *point* (second body) :attribute *h3-attribute*))

(defmethod traverse-aux ((tag (eql :h4)) body)
  (insert-string *point* (second body) :attribute *h4-attribute*))

(defmethod traverse-aux ((tag (eql :h5)) body)
  (insert-string *point* (second body) :attribute *h5-attribute*))

(defmethod traverse-aux ((tag (eql :h6)) body)
  (insert-string *point* (second body) :attribute *h6-attribute*))

(defmethod traverse-aux ((tag (eql :br)) body)
  (insert-character *point* #\newline))

(defmethod traverse-aux ((tag (eql :img)) body)
  (micros:micros-print body)
  (let* ((attribute (first body))
         (src (cadr (assoc :src attribute)))
         (alt (cadr (assoc :alt attribute))))
    (let ((data (dex:get src))
          (file (format nil "/tmp/~A.~A" (pathname-name src) (pathname-type src))))
      (let ((image (if (equal "svg" (pathname-type file))
                       (progn
                         (alexandria:write-string-into-file data file)
                         (sdl2-image:load-svg-rw file))
                       (progn
                         (alexandria:write-byte-vector-into-file data file)
                         (sdl2-image:load-image file)))))
        (insert-string *point* alt :plist (list 'lem-sdl2::image image))))))

(defmethod traverse-aux (tag body)
  (map () #'traverse (rest body)))

(defun traverse (lhtml)
  (etypecase lhtml
    (string
     (insert-string *point* lhtml))
    (cons
     (traverse-aux (first lhtml) (rest lhtml)))))
  
(defun render-html (point string)
  (let ((lhtml (chtml:parse string (chtml:make-lhtml-builder)))
        (*point* point))
    (micros:micros-print lhtml)
    (traverse lhtml)))

(defun render-markdown (point string)
  (render-html
   point
   (with-output-to-string (out)
     (3bmd:parse-string-and-print-to-stream string out))))
