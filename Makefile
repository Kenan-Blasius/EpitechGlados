##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## Makefile
##

BINARY_PATH :=  $(shell stack path --local-install-root)
GLADOS = glados
EVAL = eval
TEST_DIR = test

all:
		stack build
		cp $(BINARY_PATH)/bin/$(GLADOS)-exe ./$(GLADOS)
		cp $(BINARY_PATH)/bin/$(EVAL)-exe ./$(EVAL)

clean:
		stack clean

fclean: clean
		rm -f $(NAME)

test: $(NAME)
	@ ./$(TEST_DIR)/test.sh

re: fclean all

.PHONY: all clean fclean re