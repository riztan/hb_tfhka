# Make para ejecutar segun el sistema
# -mwindows al enlazar no sale ventana consola.
# pero para depurar, va de muerte que salgan mensajes en la consola

ifeq ($(HOME),)
   ROOT = /t-gtk/
else
   ROOT = $(HOME)/t-gtk/
endif
$(info $(ROOT)) 

SETTINGS=$(ROOT)config/settings/minimal.mk

TARGET = test
SOURCES = test.prg

LIBFILES_ +=-lhbtpathy -lhbtfhka


include $(ROOT)Rules.make

