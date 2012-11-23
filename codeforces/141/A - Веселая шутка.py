# 								A. ������� �����

# ��� � ������ ���������� ���������. ��� ���� ������ � ��� ������ ������ ����� 
# ������ � ������ ������. ����� ����������� ��� ����������� ����, �� �� ��������� 
# � ����� ������ ��������������� ������� �������� �� ������� ����� ���� ����� � �������, 
# � ���������� ��� �������� ������. ������� �����, ����� ��� ����� �����, ���-�� ���� 
# ��� ����� ���� ����� ����������. ����� ��, ��������, ��������� ��� �����, � ������� � 
# ���� ����� ����� ������.
#
# ������ ��� � �� ������� ����� ��������� ����������, �� ���� ������������� ��� ���� 
# ������: ����� �� �� ����, ��������� ����� ������, ������ ��������� ����� ����� � 
# �������? �� ���� ����� ���������, ��� �� ��������� ������, � �� �������� �������� 
# �������������� �����.
#
# �������� ����������� ����� � �� ������� ����������� � ���� ���������, ���� ��� ���� 
# ��� �������, �������� ��� �������� ������ �������, � ����� � �����, ��������� ����� 
# �������� ������ �����.
#
# ������� ������
#
# �� ������� ������ ��� ������: � ������ ������ � ��� �����, �� ������ � ��� ������� 
# ����������, � ������� ������ � ����� � �����, ��������� ����� ������ �����. ��� ������ 
# �� ����� � ������� ������ �� ��������� ��������� ����. ����� ������ ������ �� 
# ����������� 100.
#
# �������� ������
#
# �������� �YES� ��� �������, ���� �� ���� � ����� ����� ��������� ����� ����������� �����, 
# � �NO� ��� ������� � ��������� ������.
#
# ������� ������
#
# ������� ������
# SANTACLAUS
# DEDMOROZ
# SANTAMOROZDEDCLAUS
#
# �������� ������
# YES
#
# ������� ������
# PAPAINOEL
# JOULUPUKKI
# JOULNAPAOILELUPUKKI
#
# �������� ������
# NO
#
# ������� ������
# BABBONATALE
# FATHERCHRISTMAS
# BABCHRISTMASBONATALLEFATHER
#
# �������� ������
# NO

first_name  = raw_input( )
second_name = raw_input( )
heap        = raw_input( )

first_second_list = list( first_name + second_name )
heap_list         = list( heap )

first_second_list.sort( )
heap_list.sort( )

if len( first_second_list ) == len( heap_list ) and all( first_second_list[i] == heap_list[i] for i in range(len(heap_list)-1) ):
   print 'YES'
else:
   print 'NO'
