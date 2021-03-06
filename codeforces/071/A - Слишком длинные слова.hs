--                                A. ������� ������� �����
--
--     ������ ��������� ����� ����� �localization� ��� �internationalization� 
--  ��������� ������, ��� �� ������ ����������� ������ ����� ��� � ����� 
--  ���� ������.
--     ����� ������� ����� ������� �������, ���� ��� ����� ������ ������ 
--  10 ��������. ��� ������� ������� ����� ����� �������� ����������� 
--  �������������.��� ������������ �������� ��������� �������: ������������
--  ����������� � ��������� ����� �����, � ����� ���� � ���������� ���� ����� 
--  ������ � ��������� ������ (� ���������� ������� ��������� � ��� 
--  ������� �����).
--  ����� �������, �localization� ��������� ��� �l10n�, � �internationalization� 
--  ��� �i18n�.
--     ��� ������������ ���������������� ������� ������ ���� �� ������������. 
--  ��� ���� ��� ������� ������� ����� ������ ���� �������� �������������, 
--  � �����, �� ���������� ������� ��������, ������ �������� ��� ���������.
--
--  ������� ������
--
--     � ������ ������ ���������� ����� ����� n (1 <= n <= 100). � ������ �� 
--  ����������� n ����� ���������� �� ������ �����. ��� ����� ������� �� 
--  ����� ��������� ���� � ����� ����� �� 1 �� 100 ��������.
--
--  �������� ������
--
--     �������� n �����. � i ������ ������ ��������� ��������� ������ i-�� ����� 
--  �� ������� ������.
--
--  ������� ������
--
--  ������� ������
--
--        4
--        word
--        localization
--        internationalization
--        pneumonoultramicroscopicsilicovolcanoconiosis
--
--  �������� ������
--
--     word
--     l10n
--     i18n
--     p43s

main = do
	nums <- getLine
	ls <- getContents
	mapM_ (putStrLn . shrinkWord) $ words ls 
	
shrinkWord s	| length s <= 10 	= s
				| otherwise 		= [ head s ] ++ ( show $ ( length s ) - 2 ) ++ [ last s ]