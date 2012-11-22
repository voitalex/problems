#                           A. Быстрый математик
#
#   Шапур был очень способным студентом. Ему хорошо давались все науки: 
#   комбинаторика, алгебра, теория чисел, геометрия и все остальные. При
#   этом он был не только умным, но и чрезвычайно быстрым! Он мог складывать 
#  	1018 чисел всего за одну секунду.
#
#   Однажды в 230 году н. э. Шапур забеспокоился, не может ли кто-нибудь 
#   считать быстрее него. Он решил провести соревнование, в котором мог 
#   участвовать любой.
#
#   На соревновании он раздал участникам много разных пар чисел. Каждое число 
#   состояло из цифр 0 и 1. Участник в соответствие с данной ему парой чисел 
#   должен получить третье. Правило простое: i-ая цифра ответа равна 1 тогда 
#   и только тогда, когда i-ые цифры двух данных чисел отличаются. Иначе i-ая 
#   цифра ответа — 0.
#
#   Шапур подготовил много чисел и сначала решил проверить собственную скорость. 
#   Он понял, что может выполнять эти операции для чисел длины ∞ в мгновенье ока
#   (длина числа — это количество цифр в нем)! Шапур всегда вычисляет абсолютно
#   верно, и от участников своего соревнования ждет того же. Он честный человек,
#   поэтому никогда не даст никому слишком большие числа, и он всегда дает одному
#   человеку числа одинаковой длины.
#
#  	Сейчас вы примете участие в соревновании Шапура. Посмотрим, кто быстрее!
#
#   Входные данные
#
#   Входные данные состоят из двух строк. В каждой содержится одно число. 
#   Гарантируется, что числа состоят только из цифр 0 и 1 и имеют одинаковую длину.
#   Числа могут начинаться с 0. Длина чисел не превосходит 100.
#
#   Выходные данные
#
#   Выведите соответствующий ответ. Обязательно выводите лидирующие нули.
#
#   Примеры тестов
#
#   Входные данные
#      1010100
#      0100101
#
#   Выходные данные
#      1110001
#
#   Входные данные
#      000
#      111
#
#   Выходные данные
#      111
#
#   Входные данные
#      1110
#      1010
#
#   Выходные данные
#      0100
#
#   Входные данные
#      01110
#      01100
#
#   Выходные данные
#      00010

def tuple2int( x ):
   if x[0] == x[1]:
      return '0'
   else:
      return '1'

print ''.join( map( tuple2int, zip( raw_input(), raw_input() ) ) )

