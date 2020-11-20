;;; org-static-blog-custom.el --- user customization file    -*- no-byte-compile: t -*-

(defun org-static-blog-generate-post-path (post-filename post-datetime)
  (concat "html/" (file-name-nondirectory post-filename)))

(setcdr (assoc "en"
               (assoc 'date-format
                      org-static-blog-texts))
        "%Y-%m-%d")

(defun org-static-blog-get-post-summary (post-filename)
  "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
  (concat
   "<h2 class=\"post-title\">"
   (format-time-string (org-static-blog-gettext 'date-format) (org-static-blog-get-date post-filename))
   "&ensp;"
   "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
   "</h2>"))

(defun org-static-blog-post-preamble (post-filename)
  "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (let ((created-date (format-time-string (org-static-blog-gettext 'date-format)
                                          (org-static-blog-get-date post-filename)))
        (updated-date (format-time-string (org-static-blog-gettext 'date-format)
                                          (nth 5 (file-attributes post-filename)))))
    (concat
     "<h1 class=\"post-title\">"
     "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
     "</h1>\n"
     "<div class=\"post-date\"> Posted: " created-date
     (if (string-equal created-date updated-date)
         ""
       (format "&ensp;Updated: %s" updated-date))
     "</div>")))

(setq org-static-blog-page-header
      "<meta name=\"author\" content=\"ZSXH\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"/static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<script src=\"/static/katex.min.js\"></script>
<script src=\"/static/auto-render.min.js\"></script>
<link rel=\"stylesheet\" href=\"/static/katex.min.css\">
<script>document.addEventListener(\"DOMContentLoaded\", function() { renderMathInElement(document.body); });</script>
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq org-static-blog-page-preamble
      "<div class=\"header\">
<a href=\"/index.html\">Home</a> |
<a href=\"/archive.html\">Archive</a> |
<a href=\"/html/resource.html\">Resources</a> |
<a href=\"https://github.com/zsxh\">Github</a> |
<a href=\"/about.html\">About</a> |
<a href=\"/rss.xml\">RSS</a>
</div>")

(setq org-static-blog-page-postamble
      "
<center><button id=\"disqus_button\" onclick=\"load_disqus()\">Load Disqus Comments</button></center>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
    function load_disqus() {
        var dsq = document.createElement('script');
        dsq.type = 'text/javascript';
        dsq.async = true;
        dsq.src = 'https://zsxhbnbvb.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        document.getElementById('disqus_button').style.visibility = 'hidden';
    };
</script>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">zsxh.github.io</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://zsxh.github.io\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">ZSXH</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

(defun org-static-blog-get-preview (post-filename)
  "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML.
If the HTML body contains multiple paragraphs, include only the first paragraph,
and display an ellipsis.
Preamble and Postamble are excluded, too."
  (with-temp-buffer
    (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
    (let ((post-title)
          (post-date)
          (post-taglist)
          (post-ellipsis "")
          (first-paragraph-start)
          (first-paragraph-end))
      (setq post-title (org-static-blog-get-title post-filename))
      (setq post-date (org-static-blog-get-date post-filename))
      (setq post-taglist (org-static-blog-post-taglist post-filename))
      ;; Find where the first paragraph ends and starts
      (goto-char (point-min))
      (when (search-forward "<p>" nil t)
        (search-forward "</p>")
        (setq first-paragraph-end (point))
        (search-backward "<p>")
        (setq first-paragraph-start (point))
        (goto-char first-paragraph-end)
        (when (search-forward "<p>" nil t)
          (setq post-ellipsis (concat (when org-static-blog-preview-link-p
                                        (format "<a href=\"%s\">" (org-static-blog-get-post-url post-filename)))
                                      org-static-blog-preview-ellipsis
                                      (when org-static-blog-preview-link-p "</a>\n")))))
      ;; Put the substrings together.
      (concat
       "<div class=\"index-content\">"
       (format "<h2 class=\"post-title\"><a href=\"%s\">%s</a></h2>" (org-static-blog-get-post-url post-filename) post-title)
       (format-time-string (concat "<div class=\"post-date\">" (org-static-blog-gettext 'date-format) "</div>") post-date)
       (buffer-substring-no-properties first-paragraph-start first-paragraph-end)
       post-ellipsis
       (format "<div class=\"taglist\">%s</div>" post-taglist)
       "</div>"))))

(defun org-static-blog-assemble-archive ()
  "Re-render the blog archive page.
The archive page contains single-line links and dates for every
blog post, but no post body."
  (let ((archive-filename (concat-to-dir org-static-blog-publish-directory org-static-blog-archive-file))
        (archive-entries nil)
        (post-filenames (org-static-blog-get-post-filenames)))
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p
                                                             (org-static-blog-get-date y)
                                                             (org-static-blog-get-date x)))))
    (org-static-blog-with-find-file
     archive-filename
     (concat
      "<!DOCTYPE html>\n"
      "<html lang=\"" org-static-blog-langcode "\">\n"
      "<head>\n"
      "<meta charset=\"UTF-8\">\n"
      "<link rel=\"alternate\"\n"
      "      type=\"application/rss+xml\"\n"
      "      href=\"" (org-static-blog-get-absolute-url org-static-blog-rss-file) "\"\n"
      "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
      "<title>" org-static-blog-publish-title "</title>\n"
      org-static-blog-page-header
      "</head>\n"
      "<body>\n"
      "<div id=\"preamble\" class=\"status\">\n"
      org-static-blog-page-preamble
      "</div>\n"
      "<div id=\"archive-content\">\n"
      "<h1 class=\"title\">" (org-static-blog-gettext 'archive) "</h1>\n"
      (apply 'concat (mapcar 'org-static-blog-get-post-summary post-filenames))
      "</div>\n"
      "<div id=\"postamble\" class=\"status\">"
      org-static-blog-page-postamble
      "</div>\n"
      "</body>\n"
      "</html>"))))
