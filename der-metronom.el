(require 'request)
(require 'json)

(setq stations '(("Alfeld(Leine)" . "HALF")
                 ("Ashausen" . "AASN")
                 ("Bad Bevensen" . "ABVS")
                 ("Banteln" . "HBAN")
                 ("Bardowick" . "ABAD")
                 ("Bardowick Hp" . "ABAW")
                 ("Bienenbüttel" . "ABIL")
                 ("Bremen Hbf" . "HB")
                 ("Bremen-Oberneuland" . "HBON")
                 ("Buchholz(Nordheide)" . "ABLZ")
                 ("Celle" . "HC")
                 ("Einbeck-Salzderhelden" . "HEB")
                 ("Elze(Han)" . "HELZ")
                 ("Eschede" . "HESD")
                 ("Freden(Leine)" . "HFRE")
                 ("Göttingen" . "HG")
                 ("Großburgwedel" . "HGBW")
                 ("Hamburg-Harburg" . "AHAR")
                 ("Hamburg Hbf" . "AH")
                 ("Hannover Hbf" . "HH")
                 ("Hittfeld" . "AHIF")
                 ("Isernhagen" . "HIHG")
                 ("Klecken" . "AKC")
                 ("Kreiensen" . "HK")
                 ("Langenhagen Mitte" . "HLGM")
                 ("Lauenbrück" . "ALUB")
                 ("Lüneburg" . "ALBG")
                 ("Maschen" . "AMA")
                 ("Meckelfeld                            Hp" . "AMDH")
                 ("Nordstemmen" . "HNOS")
                 ("Nörten-Hardenberg" . "HNTH")
                 ("Northeim(Han)" . "HN")
                 ("Ottersberg(Han)" . "AOBG")
                 ("Radbruch" . "ARH")
                 ("Rotenburg(Wümme)" . "AROG")
                 ("Sagehorn" . "ASAG")
                 ("Sarstedt" . "HSRD")
                 ("Scheeßel" . "ASL")
                 ("Sottrum" . "AS")
                 ("Sprötze" . "ASP")
                 ("Stelle" . "ASTE")
                 ("Suderburg" . "HSUD")
                 ("Tostedt" . "ATST")
                 ("Uelzen" . "HU")
                 ("Unterlüß" . "HUNL")
                 ("Winsen(Luhe)" . "AWI")))

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

(defun abfahrt-decorator (prognose)
  (cond
   ((string= prognose "pünktlich") 'italic)
   ((string= prognose "Fahrt fällt aus!") 'error)
   (t 'warning)))

(defun format-abfahrt (abfahrt)
  (let* ((zeit (cdr (assoc 'zeit abfahrt)))
         (ziel (cdr (assoc 'ziel abfahrt)))
         (prognose (cdr (assoc 'prognose abfahrt)))
         (face (abfahrt-decorator prognose)))
    (list nil (vector
               (propertize (format "%s Uhr" zeit) 'face face)
               (propertize ziel 'face face)
               (propertize prognose 'face face)))))

(defun get-abfahrten ()
  (let* ((from (if (boundp 'metronom-from) metronom-from "Hamburg Hbf"))
         (to (if (boundp 'metronom-to) metronom-to "Lüneburg"))
         (response (query-for from))
         (abfahrten (cdr (assoc 'abfahrt response)))
;         (abfahrten (seq-filter (station-filter to) (cdr (assoc 'abfahrt response))))
         )
    (mapcar 'format-abfahrt abfahrten)))

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
  (add-hook 'tabulated-list-revert-hook 'der-metronom--refresh nil t))


