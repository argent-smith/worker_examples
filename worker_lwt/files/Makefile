files:
	number=1 ; while [[ $$number -le 10 ]] ; do \
	  dd if=/dev/random of=$$number.dat count=1024 bs=1024 ; \
	  ((number = number + 1)) ; \
	done

clean:
	rm -f *.dat
