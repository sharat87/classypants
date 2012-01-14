default: up

up:
	scp -r site/* wf:~/webapps/classypants_project/
	notify-send 'Classypants site updated'
