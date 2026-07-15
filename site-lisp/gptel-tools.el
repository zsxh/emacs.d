;; gptel-tools.el --- gptel tools	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;; see [gptel tools coolection](https://github.com/karthink/gptel/wiki/Tools-collection)
;;

;;; Code:

(require 'gptel)
(require 'plz)

(require 'cl-lib)
(require 'seq)


(defun gptel-tools-youtube-metadata (callback url)
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
 :function #'gptel-tools-youtube-metadata
 :description "Find the description and video transcript for a youtube video. Return a XML object containing two fields:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
          :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
          :type string))
 :category "youtube"
 :async t
 :include t)

(gptel-make-tool
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min))
               (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (shr-insert-document dom)
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
               :type string
               :description "The URL to read"))
 :category "web")

(gptel-make-tool
 :function (lambda (url)
             ;; NOTE: [html-to-markdown](https://github.com/xberg-io/html-to-markdown)
             ;; $ html-to-markdown --url https://example.com
             (let* ((temp-buffer (generate-new-buffer " *html-to-markdown-output*"))
                    (exit-code (call-process "html-to-markdown" ; 程序名
                                             nil ; 无标准输入
                                             temp-buffer ; 输出写入该buffer
                                             nil ; 不重定向stderr（默认合并）
                                             "--url"
                                             url)))
               (unwind-protect
                   (if (zerop exit-code)
                       (with-current-buffer temp-buffer
                         (buffer-string)) ; 成功，返回内容
                     (error "命令执行失败，退出码: %d，请检查 html-to-markdown 是否安装" exit-code))
                 (kill-buffer temp-buffer))))
 :name "read_url_md"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
               :type string
               :description "The URL to read"))
 :category "web")

;; TODO: [toon format](https://github.com/toon-format/toon)
(gptel-make-tool
 :function (lambda (q pageno time_range)
             (let* ((req-url (url-encode-url
                              (format "http://localhost:8888/search?q=%s&format=json%s%s"
                                      q
                                      (if pageno (format "&pageno=%d" pageno) "")
                                      (if time_range (format "&time_range=%s" time_range) ""))))
                    (resp (plz 'get req-url))
                    (json (json-parse-string resp :object-type 'plist))
                    (results (plist-get json :results)))
               (with-temp-buffer
                 (seq-doseq (item results)
                   (insert
                    (format "Title: %s\nContent: %s\nURL: %s\nScore: %s\n\n"
                            (plist-get item :title)
                            (plist-get item :content)
                            (plist-get item :url)
                            (plist-get item :score))))
                 (buffer-string))))
 :name "web_search_searxng"
 :description "Searches the web using SearXNG and returns a list of results, each with a title, URL, and content snippet."
 :args (list '(:name "q"
               :type string
               :description "The search query string.")
             '(:name "pageno"
               :type number
               :description "Search page number (starts at 1)"
               :default 1 :optional t)
             '(:name "time_range"
               :type string
               :description "Time range of search (day, week, month, year)"
               :enum ["day" "week" "month" "year"] :optional t)
             ;; '(:name "format"
             ;;   :type string
             ;;   :description "Output format of results."
             ;;   :enum ["json" "csv" "rss"] :optional t)
             )
 :category "web")


(provide 'gptel-tools)

;;; gptel-tools.el ends here
