main:
	sicstus -l ll345374.pl --goal "verify(8, 'mb2.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'petersan.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(-1, 'peterson.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'peterson.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'unsafe.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'zeroIndex.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(4, 'testkos.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(5, 'testkos.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'only2.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'dek.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'mb2.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'mb3.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(1, 'mb3.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'hyman.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(3, 'only2.txt'), halt." --nologo --noinfo
	sicstus -l ll345374.pl --goal "verify(2, 'mb1.txt'), halt." --nologo --noinfo
