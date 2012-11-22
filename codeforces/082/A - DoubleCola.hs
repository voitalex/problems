--                                 A. Double Cola
--
-- ������, �������, �����, ������ � ������ ����� � ������� � �������� 
-- �� ������� ������� � �������� �Double Cola�, ������ ����� � ������� 
-- ���. ������ � ������� (������) �������� �������, �������� �� ���������� 
-- � �������������! ������������ ��� ������� ������ � ����� �������. ����� 
-- ��������� � ������� (�������) �������� �������, �������� � ������ � 
-- ����� ������� � ������� ����������, � ��� �����. ���� ������� ������������ 
-- �� �������������.
--
-- ��������, ������ ������� ���� ������ �����, � ������� ����� ��������� ���: 
-- ������, ������, ������, ������, �������, �������, �����, �����.
--
-- �������� ���������, ������� ������� ��� ��������, ��������� n-�� �������.
--
-- �������� ��������, ��� � ����� ������ ������� �������� ���: ������, 
-- �������, �����, ������, ������. ������ ��������� �������� ������.
--
-- ������� ������
--
-- ������� ������ ������� �� ������������� ������ ����� n (1???n???109).
--
-- �������������, ��� � ��������� ����������� ������������ ��������� ���� 
-- ���� ����, �� ���� � ��� ����������� ��� ���� ��������� �������.
--
-- �������� ������
--
-- �������� ������������ ������ � ��� ��������, ������� ������ n-�� ������� 
-- ����. ������� ���������� �� 1. �������� ��������, ��� ������� �������� 
-- ����� � ��������� ���������: "Sheldon", "Leonard", "Penny", "Rajesh", 
-- "Howard" (��� �������). ������ � ���� ������� ������ ����� � ������� 
-- ����������.
--
-- ������� ������
--
-- ������� ������
--  1
--
-- �������� ������
--  Sheldon
--
-- ������� ������
--  6
--
-- �������� ������
--  Sheldon
--
-- ������� ������
--  1802
--
-- �������� ������
--  Penny

toInt :: String -> Int
toInt = read

personIdRange :: Int -> [ ( Int, Int ) ]
personIdRange n	=  	zip ( scanl (+) 1 (lst 0) ) ( scanl (+) n (lst 1) )
					where lst from 	= [ n * 2^x  | x <- enumFrom from ]

between :: Int -> ( Int, Int ) -> Bool
between x (left, right) = if left <= x && x <= right then True else False

nthPerson :: Int -> String
nthPerson n = nameList !! ( findPerson (personIdRange 5) )
			where
				nameList 			= [ "Sheldon", "Leonard", "Penny", "Rajesh", "Howard" ]
				findPerson (x:xs)	| between n x 	= ( 5 * (n - fst x) ) `div` (snd x - fst x + 1)
									| otherwise 	= findPerson xs	
main = do
	str <- getLine
	putStrLn $ ( nthPerson  . toInt ) str





