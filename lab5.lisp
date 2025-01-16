(in-package :cl-user)

(defstruct manufacturer
  id
  name
  country
  established-year)

(defstruct drone
  id
  name
  type
  manufacturer-id
  description)

(defun manufacturer-slots ()
  '(id name country established-year))

(defun drone-slots ()
  '(id name type manufacturer-id description))

(defun my-split-sequence (delimiter string)
  (let ((start 0)
        (result '()))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (push (subseq string start pos) result)
             (setf start (1+ pos)))
    (push (subseq string start) result)
    (nreverse result)))

(defun create-manufacturer-record (row)
  (let* ((fields (my-split-sequence #\, row)))
    (if (= (length fields) 4)
        (make-manufacturer
         :id (parse-integer (nth 0 fields))
         :name (nth 1 fields)
         :country (nth 2 fields)
         :established-year (parse-integer (nth 3 fields)))
        (progn
          (format t "Error: Invalid row ~a~%" row)
          nil))))

(defun create-drone-record (row)
  (let* ((fields (my-split-sequence #\, row)))
    (if (= (length fields) 5)
        (make-drone
         :id (parse-integer (nth 0 fields))
         :name (nth 1 fields)
         :type (nth 2 fields)
         :manufacturer-id (parse-integer (nth 3 fields))
         :description (nth 4 fields))
        (progn
          (format t "Error: Invalid row ~a~%" row)
          nil))))

(defun read-csv-file (file-path create-record-fn)
  (with-open-file (stream file-path :if-does-not-exist nil)
    (if stream
        (let ((header (read-line stream nil)))
          (loop for line = (read-line stream nil)
                while line
                collect (funcall create-record-fn line)))
        (progn
          (format t "Error: Cannot read file ~a~%" file-path)
          nil))))

(defun select (file-path create-record-fn)
  (let ((records (read-csv-file file-path create-record-fn)))
    (lambda (&rest filters)
      (let ((filtered-records records))
        (dolist (filter filters)
          (let ((field (car filter))
                (value (cdr filter)))
            (setf filtered-records
                  (remove-if-not
                   (lambda (record)
                     (equal value (slot-value record field)))
                   filtered-records))))
        filtered-records))))

(defun write-records-to-csv (file-path records)
  (with-open-file (stream file-path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (record records)
      (let ((slots (cond
                    ((typep record 'manufacturer) (manufacturer-slots))
                    ((typep record 'drone) (drone-slots)))))
        (format stream "~{~a~^,~}~%" 
                (mapcar (lambda (slot) (slot-value record slot)) slots))))))

(defun pretty-print-records (records)
  (dolist (record records)
    (let ((slots (cond
                  ((typep record 'manufacturer) (manufacturer-slots))
                  ((typep record 'drone) (drone-slots)))))
      (format t "Record of type ~a:~%" (type-of record))
      (dolist (slot slots)
        (format t "  ~a: ~a~%" slot (slot-value record slot)))
      (format t "~%"))))

(defun structure-to-hashtable (record)
  (let ((hash (make-hash-table))
        (slots (cond
                ((typep record 'manufacturer) (manufacturer-slots))
                ((typep record 'drone) (drone-slots)))))
    (dolist (slot slots)
      (setf (gethash slot hash) (slot-value record slot)))
    hash))


;;; Зчитування даних
(defvar *manufacturers* (read-csv-file "manufacturers.csv" #'create-manufacturer-record))
(defvar *drones* (read-csv-file "drones.csv" #'create-drone-record))

;;; Вивід даних
(format t "Manufacturers and Drones:~%")
(pretty-print-records *manufacturers*)
(pretty-print-records *drones*)

;;; Приклад фільтрації
(let ((select-drones (select "drones.csv" #'create-drone-record)))
  (let ((filtered-drones (funcall select-drones '(type . "Quadcopter"))))
    (if filtered-drones
        (progn
          (format t "Filtered Drones:~%")
          (pretty-print-records filtered-drones)
          (write-records-to-csv "filtered_drones.csv" filtered-drones))
        (format t "No drones found for type = Quadcopter~%"))))
