(deftemplate person 
	(slot name) 
	(slot age) 
	(slot eye-color) 
	(slot hair-color))

(assert (person 
	(name "John Q. Public") 
	(age 23) 
	(eye-color blue) 
	(hair-color black)))

