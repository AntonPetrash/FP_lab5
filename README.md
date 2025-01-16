## МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС

### Звіт з лабораторної роботи 5
 "Робота з базою даних"
 дисципліни "Вступ до функціонального програмування"

**Студент**: *Петраш Антон Степанович КВ-13*


**Рік**: *2025*

## Завдання:
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

Варіант 13(1): 
База даних: Виробництво дронів
Тип записів: Структура
Таблиці: ВИробники дронів, Дрони
Опис: База даних виробників дронів та, власне, дронів.

**Код завдання:**
```
(in-package :cl-user)

;;; Структури даних
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

;;; Слоти структур
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

```
**Тестові файли:**
"Drones.CSV:"
```
ID,Name,Type,Manufacturer ID,Description
1,Mavic Air 2,Quadcopter,1,Compact and powerful drone for photography.
2,ANAFI,Quadcopter,2,Foldable drone with 4K HDR video.
3,Puma 3 AE,Fixed-Wing,3,Military-grade drone for surveillance.
4,EVO Lite,Quadcopter,4,Advanced drone with superior flight range.
5,X2D,Quadcopter,5,AI-driven drone for autonomous missions.
```
"Manufacturers.CSV:"
```
ID,Name,Country,Established Year
1,DJI,China,2006
2,Parrot,France,1994
3,AeroVironment,USA,1971
4,Autel Robotics,USA,2014
5,Skydio,USA,2014
```
**виведення диних:**
```
Manufacturers and Drones:
Record of type MANUFACTURER:
  ID: 1
  NAME: DJI
  COUNTRY: China
  ESTABLISHED-YEAR: 2006

Record of type MANUFACTURER:
  ID: 2
  NAME: Parrot
  COUNTRY: France
  ESTABLISHED-YEAR: 1994

Record of type MANUFACTURER:
  ID: 3
  NAME: AeroVironment
  COUNTRY: USA
  ESTABLISHED-YEAR: 1971
```
