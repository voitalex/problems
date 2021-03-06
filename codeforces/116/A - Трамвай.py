#                              A. Трамвай
#
# В Линейном Королевстве всего один трамвайный маршрут. На нем n остановок, 
# пронумерованных от 1 до n в порядке следования трамвая. На i-ой остановке 
# ai человек выходит из трамвая, а bi человек заходит в трамвай. Трамвай 
# прибывает на первую остановку пустым. Также, когда трамвай прибывает на 
# последнюю остановку, все пассажиры выходят, и трамвай уезжает пустым.
#
# Ваша задача — найти минимальную возможную вместимость трамвая, такую, что 
# количество пассажиров в трамвае в любой момент времени не превосходит эту 
# вместимость. Учтите, что на каждой остановке все пассажиры выходят до того 
# как какой-либо пассажир заходит.
#
# Входные данные
# 	В первой строке записано целое число n (2 <= n <= 1000) — количество 
# остановок трамвая.
#
# Далее следует n строк, в каждой — по два целых числа ai и bi 
# (0 <= ai, bi <= 1000) — количество пассажиров, которые выходят из трамвая 
# на i-ой остановке, и количество пассажиров, которые заходят в трамвай на 
# i-ой остановке. Остановки перечислены в том же порядке, в котором их 
# проезжает трамвай.
#
# Количество пассажиров, которые выходят на остановке, не превосходит общего 
# количества пассажиров в трамвае в момент, когда он подъезжает к этой 
# остановке.
#  
# Выходные данные
# Выведите одно целое число — минимальную возможную вместимость трамвая. 
# Допускается, что вместимость может быть равна нулю.
#   
# Примеры тестов
#
# Входные данные
#     4
#     0 3
#     2 5
#     4 2
#     4 0
#
# Выходные данные
#     6

from string import split

def find_pasg_max( curr, next ):
   tmp = curr[1] + next[1] - next[0] 
   return ( max( curr[0], tmp ), tmp )

N           = int( raw_input( ) )
passengers  = []

for i in range( N ):
   passengers.append ( tuple( map( int, split( raw_input( ) ) ) ) )

print reduce( find_pasg_max, passengers, ( 0, 0 ) )[ 0 ]

