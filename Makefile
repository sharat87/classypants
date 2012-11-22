default: up

up:
	rsync --delete --recursive site/* wf:~/webapps/classypants_project/
	notify-send 'Classypants site updated'
