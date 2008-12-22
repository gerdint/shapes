SUBDIRS = features applications misc showcase

all:
	for d in $(SUBDIRS); do\
		echo "Running make in" $$d "...";\
		( cd $$d; make )\
	done
