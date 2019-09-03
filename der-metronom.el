(require 'request)
(require 'json)

(setq stations '(("Lüneburg" . "ALBG")
                 ("Bardowick" . "ABAD")
                 ("Radbruch" . "ARH")
                 ("Winsen" . "AWI")
                 ("Ashausen" . "AASN")
                 ("Stelle" . "ASTE")
                 ("Maschen" . "AMA")
                 ("Meckelfeld" . "AMDH")
                 ("Hamburg-Harburg" . "AHAR")
                 ("Hamburg Hbf" . "AH")))

(defun query-for (station)
  (let* ((station-code (cdr (assoc station stations))))
    (message "Hole die Abfahrten von %s" station station-code)
    (request-response-data (request "https://www.der-metronom.de"
                                    :success (cl-function
                                              (lambda (&key data &allow-other-keys)
                                                (message "Request success!")))
                                    :sync t))
    (request-response-data (request "https://www.der-metronom.de/livedata/etc"
                                    :params (list (cons "type" "stationsauskunft")
                                                  (cons "product" "metronom")
                                                  (cons "bhf" station-code))
                                    :parser 'json-read
                                    :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                                          (message "Got error: %S" error-thrown)))
                                    :sync t))))

(defun station-filter (station)
  `(lambda (element) (string= (cdr (assoc-string 'ziel element)) ,station)))

(defun get-abfahrten ()
  (let* ((from (if (boundp 'metronom-from) metronom-from "Hamburg Hbf"))
         (to (if (boundp 'metronom-to) metronom-to "Lüneburg"))
         (response (query-for from))
         (abfahrten (cdr (assoc 'abfahrt response)))
;         (abfahrten (seq-filter (station-filter to) (cdr (assoc 'abfahrt response))))
         )
    (mapcar 
     (lambda (el) 
       (list nil (vector
                  (format "%s Uhr" (cdr (assoc 'zeit el)))
                  (cdr (assoc 'ziel el))
                  (cdr (assoc 'prognose el))))) 
     abfahrten)))

(defun der-metronom--refresh ()
  (setq tabulated-list-entries (get-abfahrten))
  (tabulated-list-init-header))

(defun der-metronom (&optional buffer)
  (interactive)
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create (format "*Abfahrten %s*" metronom-from))))
  (with-current-buffer buffer
    (metronom-mode)
    (der-metronom--refresh)
    (tabulated-list-print))
  (pop-to-buffer buffer)
  nil)


(define-derived-mode metronom-mode tabulated-list-mode "Metronom"
  "Major Mode zum Anzeigen von Metronom Vespätungen"
  (setq tabulated-list-format [("Abfahrtszeit" 15 t)
                               ("Ziel" 15 t)
                               ("Prognose" 15 t)])
  (add-hook 'tabulated-list-revert-hook 'metronom--refresh nil t))


