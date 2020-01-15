;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Lab 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Edwin J. Ortiz
;; Lab 8

(require picturing-programs)
(require "lab8-extras.rkt")

;;PROBLEM 1
;; iron-decode: image -> image
;; consumes: an image
;; produces: an image where the green and blue pixels is reduced to 0 and red is multiplied x10
(check-expect (get-pixel-color 0 0
                               (iron-decode
                                (color-list->bitmap (list (make-color 1 1 1)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 10 0 0))
(check-expect (get-pixel-color 0 0
                               (iron-decode
                                (color-list->bitmap (list (make-color 2 1 1)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 20 0 0))

(define (iron-decode anImage)
  (local[(define (decode-red old-color)
           (make-color (* 10 (color-red old-color)) 0 0))]
    (map-image decode-red anImage)))

;; PROBLEM 2
;; west-decode: image -> image
;; consumes: an image
;; produces: an image where the blue value is multiplied by 16 if it is less than 16,
;;           If the blue value is higher than 16, it will remain the same value.
;;           once the new blue value is found, the new blue value will replace the red value.
;;           The green and blue value will be set to zero

(check-expect (get-pixel-color 0 0
                               (west-decode
                                (color-list->bitmap (list (make-color 1 1 2)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 32 0 0))

(check-expect (get-pixel-color 0 0
                               (west-decode
                                (color-list->bitmap (list (make-color 50 50 50)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 50 0 0))

(define (west-decode anImage)
  (local[(define (true-color aColor)
           (cond [(< aColor 16) (* 16 aColor)]
                 [else aColor]))
         
         (define (decode-west old-color)
           (make-color (true-color (color-blue old-color))  0 0))]
    (map-image decode-west anImage)))

;; PROBLEM 3
;; distance : Color Color -> PosNum
;; consumes: 2 colors.
;; produces: a number representing the distance of the colors provided

(define (distance Color1 Color2)
  (sqrt (+
         (sqr (- (color-red Color2) (color-red Color1))) 
         (sqr (- (color-green Color2) (color-green Color1))) 
         (sqr (- (color-blue Color2) (color-blue Color1))))))

;; instant-alpha: Color Distance Image -> Image
;; consumes: a Color, a Distance(Color1 Color2), and an Image
;; produces: an Image where the Alpha of all the pixels whose Color is within th given Distance
;;           of the given Color is set to 0
(check-expect (get-pixel-color 0 0
                               (instant-alpha BLUE 200
                                              (color-list->bitmap (list (make-color 0 0 100)) ;one pixel
                                                                  1 1))) ;width 1, height 1
              (make-color 255 255 255 0))

(check-expect (get-pixel-color 0 0
                               (instant-alpha RED 0
                                              (color-list->bitmap (list (make-color 0 100 0)) ;one pixel
                                                                  1 1))) ;width 1, height 1
              (make-color 0 100 0 255))

(define (instant-alpha aColor aDistance anImage)
  (local [(define (range aColor old-color)
            (cond [(> aDistance (distance aColor old-color)) 
                   (make-color
                    (color-red old-color) (color-green old-color) (color-blue old-color) 0)]
                  [else old-color])) 
          
          (define (ColorAlpha old-color)
            (range aColor old-color))]
    
    (map-image ColorAlpha anImage)))

;; PROBLEM 4
;; remove-red-eye: NatNum NatNum NatNum NatNum Image -> Image
;; consumes: 4 Natnums corresponding to the bounding box. (x1 y1 x2 y2)
;; produces: the givem image with any reddish pixels in the specified box blackened
(check-expect (get-pixel-color 0 0
                               (remove-red-eye 0 0 0 0
                                               (color-list->bitmap (list (make-color 255 0 0)) ;one pixel
                                                                   1 1))) ;width 1, height 1
              (make-color 0 0 0 255))

(check-expect (get-pixel-color 0 0
                               (remove-red-eye 10 20 10 20
                                               (color-list->bitmap (list (make-color 255 0 0)) ;one pixel
                                                                   1 1))) ;width 1, height 1
              (make-color 255 0 0 255))

(check-expect (get-pixel-color 0 0
                               (remove-red-eye 0 1 0 1
                                               (color-list->bitmap (list (make-color 0 0 255)) ;one pixel
                                                                   1 1))) ;width 1, height 1
              (make-color 0 0 255 255))

(define (remove-red-eye x1 y1 x2 y2 img)
  (local [(define (within-red old-color)
            (cond [(> 165 (distance RED old-color)) 
                   (make-color
                    0 0 0)]
                  [else old-color]))
          
          (define (area-rectangle x y old-color)
            (cond [(and (<= x1 x x2) (<= y1 y y2)) (within-red old-color)]
                  [else old-color]))
          
          (define (remove-red x y old-color)
            (area-rectangle x y old-color))]
    
    (map-image remove-red img)))

; (remove-red-eye 100 90 190 130 JEN)


;; PROBLEM 5
;Part a.
;; grayscale1 : image -> image
;; consumes: an image
;; prodouces: an image where all the pixels of the given image is the avg RGB luminance of the image
(check-expect (get-pixel-color 0 0
                               (grayscale1
                                (color-list->bitmap (list (make-color 1 1 1)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 1 1 1))

(check-expect (get-pixel-color 0 0
                               (grayscale1
                                (color-list->bitmap (list (make-color 20 6 6)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 11 11 11))

(define (grayscale1 anImage)
  (local [(define (AverageLuminance old-color)
            (round (/ 
                    (+ (color-red old-color)  (color-green old-color) (color-blue old-color))
                    3)))
          (define (luminance old-color)
            (make-color (AverageLuminance old-color)
                        (AverageLuminance old-color)
                        (AverageLuminance old-color)))]
    
    (map-image luminance anImage)))

;Part b.
(check-expect (get-pixel-color 0 0
                               (grayscale2
                                (color-list->bitmap (list (make-color 1 1 1)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 1 1 1))

(check-expect (get-pixel-color 0 0
                               (grayscale2
                                (color-list->bitmap (list (make-color 9 5 4)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 6 6 6))

(define (grayscale2 anImage)
  (local [(define (PerceivedLuminance old-color)
            (round (+ 
                    (* (color-red old-color) 0.299)
                    (* (color-green old-color) 0.587)
                    (* (color-blue old-color) 0.114))))
          (define (luminance old-color)
            (make-color (PerceivedLuminance old-color)
                        (PerceivedLuminance old-color)
                        (PerceivedLuminance old-color)))]
    
    (map-image luminance anImage)))

;; PROBLEM 6
;; sepia: image -> image
;; consumes: an image
;; produces: an image recolored as sepia by changing each pixel 
(check-expect (get-pixel-color 0 0
                               (sepia
                                (color-list->bitmap (list (make-color 1 1 1)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 1 1 1))

(check-expect (get-pixel-color 0 0
                               (sepia
                                (color-list->bitmap (list (make-color 9 5 4)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color 8 7 6))

(check-expect (get-pixel-color 0 0
                               (sepia
                                (color-list->bitmap (list (make-color 255 255 255)) ;one pixel
                                                    1 1))) ;width 1, height 1
              (make-color  255 255 239))

(define (sepia anImage)
  (local [(define (clamp number)
            (cond [(> number 255) 255]
                  [else number]))
          (define (sepia-color old-color) 
            (make-color
             (round (clamp (+ (* (color-red old-color) 0.393) (* (color-green old-color) 0.769) (* (color-blue old-color) 0.189))))
             (round (clamp (+ (* (color-red old-color) 0.349) (* (color-green old-color) 0.686) (* (color-blue old-color) 0.169))))
             (round (clamp (+ (* (color-red old-color) 0.272) (* (color-green old-color) 0.534) (* (color-blue old-color) 0.131))))))]
    
    (map-image sepia-color anImage)))

;(sepia YD) 

;; PROBLEM 7
;; anaglyph: image image -> image
;; consumes: left image and right image of the same size
;; produces: one image where the red values are the same as the one's in the left image and 
;;           the green and blue images are the same as the right images

;get-pixel-color: maps one image gets the pixel of another image


(check-expect (anaglyph
               (pixel->image RED)
               (pixel->image BLUE))
              (pixel->image (make-color 255 0 255)))

(check-expect (anaglyph
               (pixel->image (make-color 150 255 255))
               (pixel->image (make-color 255 50 100)))
              (pixel->image (make-color 150 50 100)))

(define (anaglyph image-left image-right)
  (local [(define (color-image x y anImage)
            (make-color (color-red (get-pixel-color x y image-left)) 
                        (color-green (get-pixel-color x y image-right)) 
                        (color-blue (get-pixel-color x y image-right))))]
    (map-image color-image image-left)))

;; PROBLEM 8
;; white?: Color -> Boolea
;; consumes: a Color
;; produces: true if the color is white (make-color 255 255 255)
;;           false otherwise
(check-expect (white? WHITE) true)
(check-expect (white? BLACK) false)
(define (white? acolor)
  (color=? acolor WHITE))

;Part A
;; encode: Image Image -> Image
;; consumes: two images, one image and one message
;; produces: one image that each pixel is the same as the given image except for where the given message is not white
;;           where the message is not white the red chanel should be rounded up to the nearest odd value, all other reds
;;           should be rounded down to the nearest even red
(check-expect (encode
               (pixel->image BLACK)
               (pixel->image RED))
              (pixel->image (make-color 255 0 0)))

(check-expect (encode
               (pixel->image BLACK)
               (pixel->image (make-color 254 10 100)))
              (pixel->image (make-color 255 10 100)))



(check-expect (encode
               (pixel->image WHITE) 
               (pixel->image (make-color 255 0 0)))
              (pixel->image (make-color 254 0 0)))

(check-expect (encode
               (pixel->image WHITE) 
               (pixel->image (make-color 254 10 100)))
              (pixel->image (make-color 254 10 100)))


(define (encode code-image image)
  (local [(define (make-odd n)
            (if (odd? n) n (add1 n)))
          
          (define (make-even n)
            (if (even? n) n (sub1 n)))
          
          (define (color-image x y c)
            (if (white? (get-pixel-color x y code-image))
                (make-color (make-even (color-red c))
                            (color-green c)
                            (color-blue c))
                (make-color (make-odd (color-red c))
                            (color-green c)
                            (color-blue c))))]
    (map-image color-image image)))


;Part B
;; decode: Image -> Image
;; consumes: an image
;; produces: an image where every odd red color is black, otherwise white
(check-expect (decode
               (pixel->image (make-color 244 10 10)))              
              (pixel->image WHITE))

(check-expect (decode
               (pixel->image RED))
              (pixel->image BLACK))


(define (decode image)
  (local [(define (color-image x y c)
            (if (odd? (color-red c)) BLACK WHITE))]  
    (map-image color-image image)))

;; PROBLEM 9
;Part a
;; median : [NEListOf Number] -> Number
;; consumes: a non-empty list of numbers
;; produces: median value (Number). That is, sorted list and middle number, if even the average of
;;           the two numbers
(check-expect (median '(1)) 1) 
(check-expect (median '(1 3 2)) 2) 
(check-expect (median '(1 3 4 2)) 2.5)
(define (median aNelon)
  (local [(define LENGTH (length aNelon))
          (define SORT (sort aNelon <))
          (define MIDDLE (sub1 (round (/ LENGTH 2))))
          (define (median-even aNelon)
            (/ (+ (list-ref SORT MIDDLE)
                  (list-ref SORT (add1 MIDDLE))) 2))
          
          (define (median-odd aNelon)
            (list-ref SORT MIDDLE))]       
    (cond [(= 1 LENGTH) (list-ref aNelon 0)]
          [(odd? LENGTH) (median-odd aNelon)]
          [(even? LENGTH) (median-even aNelon)])))

;Part b
;; make-median-color: [ListOf Color] -> Color
;; consumes: a List of Color
;; produces: a color where the red, blue, and green values are the median values of the colors in the lists
(check-expect (make-median-color (list (make-color 255 10 10) (make-color 150 100 100) (make-color 50 70 20))) (make-color 150 70 20))
(check-expect (make-median-color (list (make-color 110 50 75) (make-color 115 35 80))) (make-color 112 42 78))
(check-expect (make-median-color (list (make-color 110 50 75))) (make-color 110 50 75))
(define (make-median-color aLoc)
  (make-color (round (median (map color-red aLoc)))
              (round (median (map color-green aLoc)))
              (round (median (map color-blue aLoc)))))

;Part c
;; median-image: [ListOf Image] -> Image
;; consumes: a List Of Images that are the same size
;; produces: an Image where every pixel is the median of the corresponding pixels

(check-expect (median-image (list (color-list->bitmap (list RED BLACK) 2 1)
                                  (color-list->bitmap (list RED RED) 2 1)
                                  (color-list->bitmap (list WHITE BLACK) 2 1)))
              (color-list->bitmap (list RED BLACK) 2 1))

(check-expect (median-image (list (color-list->bitmap (list BLACK BLACK RED) 3 1)
                                  (color-list->bitmap (list BLACK RED WHITE) 3 1)
                                  (color-list->bitmap (list WHITE BLACK RED) 3 1)))
              (color-list->bitmap (list BLACK BLACK RED) 3 1))

(define (median-image aLoi)
  (local [(define HEIGHT (image-height (first aLoi)))
          (define WIDTH (image-width (first aLoi)))
          (define image-color-list
            (map image->color-list aLoi))
          (define median->color-list 
            (map-list make-median-color image-color-list))]
    (color-list->bitmap median->color-list WIDTH HEIGHT)))


;; EXTRA CREDIT
;; main: String String Number -> Image
;; consumes: a file name (string) an extension (string) and the number of files (number)
;; produces: an Image where the images called produce a median-image
(define (main aString anExt n)
    (local[(define (number->list n)
             (build-list n add1))
           
           (define (make-file-string n)
             (string-append aString (number->string n) "." anExt))
           
           (define make-file-list
             (map make-file-string (number->list n)))
           
           (define load-image-list 
             (map bitmap/file make-file-list))
           
           (define new-image-name
             (string-append aString "-median." anExt))]
      
      (save-image (median-image load-image-list) new-image-name)))

;(main "tourist" "png" 9)