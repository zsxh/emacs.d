;; gptel-tools.el --- gptel tools	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;; see [gptel tools coolection](https://github.com/karthink/gptel/wiki/Tools-collection)
;;

;;; Code:

(require 'gptel)
(require 'cl-lib)

(defun my/gptel-youtube-metadata (callback url)
  (let* ((video-id (and (string-match (concat
                                       "^\\(?:http\\(?:s?://\\)\\)?\\(?:www\\.\\)?\\(?:youtu\\(?:\\(?:\\.be\\|be\\.com\\)/\\)\\)"
                                       "\\(?:watch\\?v=\\)?"
                                       "\\([^?&]+\\)")
                                      url)
                        (match-string 1 url)))
         (dir (file-name-concat temporary-file-directory "yt-dlp" video-id)))
    (if (file-directory-p dir)
        (delete-directory dir t))
    (make-directory dir t)
    (let ((default-directory dir)
          (idx 0)
          (data (list :description nil :transcript nil)))
      (cl-labels ((try-finalize
                   ()
                   (cl-incf idx)
                   (when (= idx 2)
                     (funcall callback (format "<video>\n<description>%s</description>\n<transcript>%s</transcript>\n</video>"
                                               (plist-get data :description)
                                               (plist-get data :transcript)))
                     (delete-directory dir t))))
        (make-process :name "yt-dlp"
                      :command `("yt-dlp" "--write-description" "--skip-download" "--output" "video" ,url)
                      :sentinel (lambda (proc status)
                                  (let* ((default-directory dir)
                                         (desc-file "video.description"))
                                    (when (file-readable-p desc-file)
                                      (plist-put data :description
                                                 (with-temp-buffer
                                                   (insert-file-contents desc-file)
                                                   (buffer-string)))))
                                  (try-finalize)))
        (make-process :name "yt-dlp"
                      :command `("yt-dlp" "--skip-download" "--write-auto-subs" "--sub-langs" "zh-*,en,-live_chat" "--convert-subs" "srt" "--output" "video" ,url)
                      :sentinel (lambda (proc status)
                                  (let* ((default-directory dir)
                                         (srt-file (car (directory-files dir t "video\\..*\\.srt$"))))
                                    (when (and srt-file (file-readable-p srt-file))
                                      (plist-put data :transcript
                                                 (with-temp-buffer
                                                   (insert-file-contents srt-file)
                                                   (buffer-string)))))
                                  (try-finalize)))))))

(gptel-make-tool
 :name "youtube_video_metadata"
 :function #'my/gptel-youtube-metadata
 :description "Find the description and video transcript for a youtube video. Return a XML object containing two fields:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
          :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
          :type string))
 :category "youtube"
 :async t
 :include t)


(provide 'gptel-tools)

;;; gptel-tools.el ends here
