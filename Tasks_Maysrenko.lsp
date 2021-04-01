(print "---Тема 1: основы языка---")

;1. Определите функцию, суммирующую элементы списка.

(defun sum (lst)
    (cond
        ((null lst) 0)
        (t (+ (car lst)
              (sum (cdr lst)))
        )
    )
)

;2. Определите функцию, возвращающую последний элемент списка.

(defun lastel (lst)
    (cond
        ((null (cdr lst)) (car lst))
        (t (lastel (cdr lst)))
    )
)

(print "8. Определите функцию, которая разделит исходный список из целых чисел на два списка: список положительных чисел и список отрицательных чисел")

;Пройти по всем элементам, сравнить их и добавить в список положительных либо отрицательных, после соединить списки

(defun conn (list1 list2)
    (cond ((null list1) list2)
        ((null list2) list1)
        (t (cons (car list1) (conn (cdr list1) list2)))
    )
)

(defun plsmns (list)
    (let ((res) (pls) (mns))
         (dolist (value list res)
             (if (> value 0)
                 (setq pls (conn pls (list value)))
             )
             (if (< value 0)
                 (setq mns (conn mns (list value)))
             )
         )
         (setq res (list pls mns))
    )
)

;Тесты:

(print (plsmns '(-7 8 5 9 -9 3 -6 -4 5 5)))
;((8 5 9 3 5 5) (-7 -9 -6 -4))
(print (plsmns '(-7 -5 0 2 -8 -6 0 10 1 -2)))
;((2 10 1) (-7 -5 -8 -6 -2)) 


(print "10. Определите функцию, осуществляющую удаление указанного количества последних элементов исходного списка.")

;Пройти по списку, пока не останется <=n элементов, тогда список пуст, вызов рекурсии с обратным добавлением элементов

(defun mnstail (n lst)
	(cond
		((<= (list-length lst) n) nil)
		(T (cons (car lst) 
                 (mnstail n (cdr lst))
           )
        )
    )
)

;Тесты:

(print (mnstail 3 '(-1 -7 1 4 0 -10 0 3 9 -7)))
;(-1 -7 1 4 0 -10 0)
(print (mnstail 8 '(8 6 3 -5 7 -9 -3 8 4 -6)))
;(8 6)

(print "13. Определите функцию, удаляющую в исходном списке все повторные вхождения элементов.")

;Если удаление из хвоста списка головы не изменяет хвост, значит голова - уникальный элемент, иначе вызываем рекурсию от голова+(хвост с удалением)

(defun deletefirst (n lst)
	(cond
		((null lst) nil)
		((equal (car lst) n) (cdr lst))
		(t (cons (car lst) (deletefirst n (cdr lst))))
	)
)

(defun deldubel (lst)
	(cond
		((null lst) nil)
		((equal (deletefirst (car lst) (cdr lst)) (cdr lst)) (cons (car lst) (deldubel (cdr lst))))
		(t
			(deldubel (cons (car lst) (deletefirst (car lst) (cdr lst))))
		)
	)
)

;Тесты:

(print (deldubel '(1 nil 1 2 1 nil)))
;(1 NIL 2)
(print (deldubel '(1 2 3 (2 1) 2 1 (nil 3))))
;(1 2 3 (2 1) (NIL 3))
(print (deldubel '(a b (a b) a c nil b)))
;(A B (A B) C NIL)

(print "21. Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.")

;Находит первое вхождение n в список и оставляет список без него

(defun deletefirst (n lst)
	(cond
		((null lst) nil)
		((equal (car lst) n) (cdr lst))
		(t (cons (car lst) (deletefirst n (cdr lst))))
	)
)

;Тесты:

(print (deletefirst '(1) '(1 2 3 (1) 2 (1 2))))
;(1 2 3 2 (1 2))
(print (deletefirst (list 1 2) '(1 2 3 (1) 2 (1 2) nil)))
;(1 2 3 (1) 2 NIL)

(print "29. Определите функцию, вычисляющую глубину списка (самой глубокой ветви).")

;Берёт максимум между глубиной для головы списка +1 и для хвоста. +1 к голове, так как она при car теряет один уровень

(defun depth (lst)
  (cond
		((atom lst) 0) 
		((null lst) 0)
        (t (max (+ 1 (depth (car lst))) (depth (cdr lst))))
	)
)

;Тесты:

(print (depth '(1 2 (3 nil) nil ((4) 5))))
;3
(print (depth '((1 2 nil) (3 4) 5)))
;2

(print "31. Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первый элемент, входящий в оба списка х и у, в противном случае NIL.")

;findel возвращает true, если элемент есть в списке. firstsame ищет первый элемент из x, который есть в y

(defun findel (a lst) 
	(if (null lst) nil 
		(if (equal a (car lst)) 
			t (findel a (cdr lst))
		)
	)
)

(defun firstsame (x y)
	(cond
		((null x) nil)
		((null y) nil)
		((eq (findel (car x) y) T) (car x))
		(t
			(firstsame (cdr x) y)
		)
	)
)

;Тесты:

(print (firstsame '(1 2 3) '((1 2) 3 (2))))
;3
(print (firstsame '(1 (2 1) 3) '((2) (3 1) nil)))
;NIL

(print "32. Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список множеством, т.е. входит ли каждый элемент в список лишь один раз.")

;findel возвращает true, если элемент есть в списке. isset возвращает true, если findel для каждого элемента возвращает nil

(defun findel (a lst) 
	(if (null lst) nil 
		(if (equal a (car lst)) 
			t (findel a (cdr lst))
		)
	)
)

(defun isset (lst)
	(cond
		((null lst) nil)
		((null (cdr lst)) t)
		((eq (findel (car lst) (cdr lst)) T) nil)
		(t
			(isset (cdr lst))
		)
	)
)

;Тесты:

(print (isset '(1 2 3 (2 1) nil)))
;T
(print (isset '(1 2 3 (2) nil (2))))
;NIL

(print "40. Определите функцию РАЗНОСТЬ, формирующую разность двух множеств, т.е. удаляющую из первого множества все общие со вторым множеством элементы.")

;findel возвращает true, если элемент есть в списке. setdiff удаляет элемент из x, если он есть в y

(defun findel (a lst) 
	(if (null lst) nil 
		(if (equal a (car lst)) 
			t (findel a (cdr lst))
		)
	)
)

(defun setdiff (x y)
	(cond
		((null x) nil)
		((eq (findel (car x) y) T) (setdiff (cdr x) y))
		(t
			(cons (car x) (setdiff (cdr x) y))
		)
	)
)

;Тесты:

(print (setdiff '(1 2) '(3 4 (2) 1)))
;(2)
(print (setdiff '(1 2 (2)) '(3 4 2)))
;(1 (2))

(print "43. Определите функцию, подсчитывающую количество всех вершин данного дерева заданной высоты.")

;Рекурсивно обходим каждую ветку, если там есть элемент, то +1. Если глубина не достигнута, а элемент - атом, то глубже идти не надо

(defun treecount (n tree)
	(cond
		((null tree) 0)
		((< n 0) 0)
		((> n 0)
			(if (atom tree) 0 (+ (treecount (- n 1) (car tree)) (treecount n (cdr tree))))
		)
		(t
			(cond
				((null tree) 0)
				(t 1)
			)
		)
	)
)

;Тесты:

(print (treecount 2 '(1 2 (3 4 (nil 5 6)) (7 8) 9 (10 nil))))
;6
(print (treecount 3 '(1 2 (3 4 (nil 5 6)) (7 8) 9 (10 nil))))
;2

(print "48. Функция GET возвращает в качестве результата NIL в том случае, если у символа нет данного свойства, либо если значением этого свойства является NIL. Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством.")

;Проверяет, есть ли свойство, удаляя его. Если удалилось, то восстанавливаем на место, иначе говорим nil

(defun hasprop (symb prop)
	(setq val (get symb prop))
	(cond
		((null (remprop symb prop)) nil)
		(T (setf (get symb prop) val) T)
	)
)

;Тесты:

(setf (get 'a 'speed) 100)
(setf (get 'a 'weight) nil)
(print (hasprop 'a 'cost))
;NIL
(print (hasprop 'a 'speed))
;T
(print (hasprop 'a 'weight))
;T

(print "---Тема 2: функции высших порядков---")

(print "1. Определите FUNCALL через функционал APPLY.")

;Просто применить f к x через apply

(defun myfuncall (f &rest x) (apply f x))

;Тесты:

(print (myfuncall '+ 3 5))
;8
(print (myfuncall '* 4 6))
;24

(print "3. Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка 
(f1 f2 ... fn) 
к соответствующему элементу списка 
x = (x1 x2 ... xn) 
и возвращает список, сформированный из результатов.")

;Применяет список функций к списку элементов, просто беря головы двух списков и применяя, а потом вызывая рекурсию от хвостов

(defun APL_APPLY (funs els)
	(cond
		((null els) nil)
        ((null funs) nil)
        (t (cons (funcall (car funs) (car els)) (APL_APPLY (cdr funs)(cdr els))))
	)
)

;Тесты:

(print (apl_apply '(numberp atom list) '(1 nil a b)))
;(T T (A))

(print "5. Определите функциональный предикат(НЕКОТОРЫй пред список),который истинен, когда являющейся функциональным аргументом предикат пред истинен хотя бы для одного элемента списка список.")

;Вызывает mapcan от списка, возвращая true, если функция p корректно отработала, и если полученный список не пустой, то результат true

(defun sometrue (p lst)
	(not (null 
		(mapcan #'(lambda (x) (if (funcall p x) (list t))) lst)
	))
)

;Тесты:

(print (sometrue 'numberp '(a b list nil 2 (2 1 3 4))))
;T
(print (sometrue 'numberp '(a b list nil (2 1 3 4))))
;NIL

(print "7. Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы, которые не обладают свойством, наличие которого проверяет предикат пред.")

;Если p для головы списка возвращает nil, то берём элемент в результат, иначе в нём есть это свойство

(defun remifnot (p lst)
	(cond
		((null lst) nil)
		((null (funcall p (car lst))) (cons (car lst) (remifnot p (cdr lst))))
		(t (remifnot p (cdr lst)))
	)
)

;Тесты:

(print (remifnot #'(lambda (x) (get x 'speed)) '(a b c)))
;(B C)
(print (remifnot #'(lambda (x) (get x 'weight)) '(a b c)))
;(A B C)

(print "9. Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...")

;Просто обычное рекурсивное решение Фибоначчи

(defun fib (&optional (n 10) (a 0) (b 1))
	(cond
		((= n 0) nil)
		(t (cons a (fib (- n 1) b (+ a b))))
	)
)

;Тесты:

(print (fib))
;(0 1 1 2 3 5 8 13 21 34)
(print (fib 4))
;(0 1 1 2)

(print "11. Определите фукнционал МНОГОФУН, который использует функции, являющиеся аргументами, по следующей схеме: 
(МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).")

;Вызывает mapcar от списка функций, возвращая список результатов применения каждой функции к x

(defun mulfun(f x)
	(mapcar #'(lambda (y) (apply y (list x))) f)
)

;Тесты:

(print (mulfun '(numberp atom list) 'a))
;(NIL T (A))
(print (mulfun '(numberp atom list) '(1 a nil)))
;(NIL NIL ((1 A NIL)))

(print "13. Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).")

;Пример самопечатающейся программы, функция принимает часть кода и подставляет его вместо x, но там такой код, что она получает собственный код

(defun quine ()
	((lambda (x)
		(list x (list (quote quote) x))
	)
	(quote 
		(lambda (x)
			(list x (list (quote quote) x))
		)
	))
)

;Тесты:

(print (quine))
;((LAMBDA (X) (LIST X (LIST (QUOTE QUOTE) X))) (QUOTE (LAMBDA (X) (LIST X (LIST (QUOTE QUOTE) X)))))
