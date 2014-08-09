#include <stdlib.h>
#include <stdio.h>

struct Masktype {
	struct Masktype *next;
	int val;
	int type;
};

struct Masktype *vtable = NULL;

int mask(int val, int type){
	if (vtable == NULL){
		vtable = malloc(sizeof(struct Masktype));
	}

	struct Masktype *current = vtable;
	int index = 0;
	while(1){
		if(current->val == val){
			return index;
		}else{
			index ++;
			if(current->next == NULL){
				break;
			}else{
				current = current->next;
			}
		}
	}

	current->next = malloc(sizeof(struct Masktype));
	current = current->next;
	current->val = val;
	current->type = type;
	current->next = NULL;

	return index;
}

int unmask(int index){
	struct Masktype *current = vtable;
	while(current != NULL){
		if(index == 0){
			return current->val;
		}
		else{
			current=current->next;
			index--;
		}
	}
	return -1;
}

int main(){
	int a = mask(0,1);
	int b = mask(5,1);
	int c = mask(3,1);
	int d = mask(5,1);
	free(vtable);	
	printf("a: %d\n",a);
	printf("b: %d\n",b);
	printf("c: %d\n",c);
	printf("d: %d\n",d);
	return 0;
}