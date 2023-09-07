;; 한국어 Lorem Ipsum 패키지

(defun korean-lorem-ipsum-sentence ()
  "Generate a Korean Lorem Ipsum sentence."
  (let* ((words '("박호열" "는" "은" "이" "가" "Lorem" "Ipsum" "한국어" "텍스트" "입력" "예시" "코드" "사랑해"))
         (sentence-length (+ 1 (random 10)))
         (korean-lorem-text ""))
    (dotimes (_ sentence-length)
      (setq korean-lorem-text (concat korean-lorem-text (nth (random (length words)) words) " ")))
    (setq korean-lorem-text (string-trim-right korean-lorem-text))
    korean-lorem-text))

(defun korean-lorem-ipsum-insert-at-point ()
  "Insert a Korean Lorem Ipsum sentence at point."
  (interactive)
  (let ((korean-lorem-text (concat (korean-lorem-ipsum-sentence) "입니다.")))
    (insert korean-lorem-text)))

;; Example usage
(global-set-key (kbd "C-c C-l") 'korean-lorem-ipsum-insert-at-point)



