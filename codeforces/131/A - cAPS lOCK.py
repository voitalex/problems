#                              	A. cAPS lOCK
#
# зАЧЕМ НУЖНА КЛАВИША cAPS lOCK?
#
# Caps Lock — клавиша компьютерной клавиатуры, предназначенная для 
# автоматической (постоянной) смены регистра букв со строчных на 
# прописные. Будучи случайно нажатой, она приводит к последствиям 
# вроде первого абзаца в условии этой задачи.
#
# Будем считать, что слово набрано с ошибочно нажатой клавишей 
# Caps Lock, если:
#  	* либо оно полностью состоит из прописных букв;
#  	* либо прописными являются все его буквы, кроме первой. 
#
# В таком случае, нужно автоматически поменять регистр всех букв на 
# противоположный. Например, регистр букв слов «hELLO», «HTTP», «z» 
# должен быть изменен.
#
# Напишите программу, которая применяет описанное выше правило или 
# оставляет слово без изменения, если оно не применимо.
#
# Входные данные
# 	В первой строке входных данных записано слово, состоящее из прописных 
# или строчных букв латинского алфавита. Длина слова — от 1 до 100 
# символов включительно.
#
# Выходные данные
# 	Выведите результат обработки данного слова.
#
# Примеры тестов
#
# Входные данные
#  cAPS
#
# Выходные данные
#  Caps
#
# Входные данные
#  Lock
#
# Выходные данные
#      Lock

word = raw_input( )

if len( word ) <= 1:
   print word.swapcase()
else:
   if word.isupper( ) or ( word[:1].islower() and word[1:].isupper() ):
      print word.swapcase()
   else:
      print word
