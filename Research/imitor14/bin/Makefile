# Generated by fake [version 5.40]


BASH_DIR := $(PWD)/fake
PWD := $(shell pwd)
ROOT_DIR := .

vpath %.mod .
vpath %.H .


imitor: $(PWD)/sim_mesh_m.o $(PWD)/shape_fns_m.o $(PWD)/post_proc_m.o $(PWD)/misc_types_m.o $(PWD)/matrix_m.o $(PWD)/lm_types_m.o $(PWD)/lin_solver_m.o $(PWD)/kinds_m.o $(PWD)/ip_strain_inc_m.o $(PWD)/ip_frc_stf_m.o $(PWD)/imitor.o $(PWD)/elems_nodes_m.o $(PWD)/constitutive_models_m.o
	@ $(BASH_DIR)/al.bash "imitor" "gfortran $(PWD)/sim_mesh_m.o $(PWD)/shape_fns_m.o $(PWD)/post_proc_m.o $(PWD)/misc_types_m.o $(PWD)/matrix_m.o $(PWD)/lm_types_m.o $(PWD)/lin_solver_m.o $(PWD)/kinds_m.o $(PWD)/ip_strain_inc_m.o $(PWD)/ip_frc_stf_m.o $(PWD)/imitor.o $(PWD)/elems_nodes_m.o $(PWD)/constitutive_models_m.o -o imitor"

$(PWD)/constitutive_models_m.o: $(PWD)/constitutive_models_m.f $(PWD)/matrix_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/constitutive_models_m.f "constitutive_models_m.mod"
$(PWD)/constitutive_models_m.mod: $(PWD)/constitutive_models_m.o

$(PWD)/elems_nodes_m.o: $(PWD)/elems_nodes_m.f $(PWD)/shape_fns_m.mod $(PWD)/matrix_m.mod $(PWD)/constitutive_models_m.mod $(PWD)/ip_strain_inc_m.mod $(PWD)/ip_frc_stf_m.mod $(PWD)/misc_types_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/elems_nodes_m.f "elems_nodes_m.mod"
$(PWD)/elems_nodes_m.mod: $(PWD)/elems_nodes_m.o

$(PWD)/imitor.o: $(PWD)/imitor.f $(PWD)/post_proc_m.mod $(PWD)/sim_mesh_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/ac.bash gfortran "-Wall -g -I." $(PWD)/imitor.f

$(PWD)/ip_frc_stf_m.o: $(PWD)/ip_frc_stf_m.f $(PWD)/matrix_m.mod $(PWD)/misc_types_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/ip_frc_stf_m.f "ip_frc_stf_m.mod"
$(PWD)/ip_frc_stf_m.mod: $(PWD)/ip_frc_stf_m.o

$(PWD)/ip_strain_inc_m.o: $(PWD)/ip_strain_inc_m.f $(PWD)/matrix_m.mod $(PWD)/misc_types_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/ip_strain_inc_m.f "ip_strain_inc_m.mod"
$(PWD)/ip_strain_inc_m.mod: $(PWD)/ip_strain_inc_m.o

$(PWD)/kinds_m.o: $(PWD)/kinds_m.f
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/kinds_m.f "kinds_m.mod"
$(PWD)/kinds_m.mod: $(PWD)/kinds_m.o

$(PWD)/lin_solver_m.o: $(PWD)/lin_solver_m.f $(PWD)/elems_nodes_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/lin_solver_m.f "lin_solver_m.mod"
$(PWD)/lin_solver_m.mod: $(PWD)/lin_solver_m.o

$(PWD)/lm_types_m.o: $(PWD)/lm_types_m.f $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/lm_types_m.f "lm_types_m.mod"
$(PWD)/lm_types_m.mod: $(PWD)/lm_types_m.o

$(PWD)/matrix_m.o: $(PWD)/matrix_m.f $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/matrix_m.f "matrix_m.mod"
$(PWD)/matrix_m.mod: $(PWD)/matrix_m.o

$(PWD)/misc_types_m.o: $(PWD)/misc_types_m.f $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/misc_types_m.f "misc_types_m.mod"
$(PWD)/misc_types_m.mod: $(PWD)/misc_types_m.o

$(PWD)/post_proc_m.o: $(PWD)/post_proc_m.f $(PWD)/misc_types_m.mod $(PWD)/sim_mesh_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/post_proc_m.f "post_proc_m.mod"
$(PWD)/post_proc_m.mod: $(PWD)/post_proc_m.o

$(PWD)/shape_fns_m.o: $(PWD)/shape_fns_m.f $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/shape_fns_m.f "shape_fns_m.mod"
$(PWD)/shape_fns_m.mod: $(PWD)/shape_fns_m.o

$(PWD)/sim_mesh_m.o: $(PWD)/sim_mesh_m.f $(PWD)/lm_types_m.mod $(PWD)/lin_solver_m.mod $(PWD)/elems_nodes_m.mod $(PWD)/misc_types_m.mod $(PWD)/kinds_m.mod
	@ $(BASH_DIR)/amac.bash gfortran "-Wall -g -I." $(PWD)/sim_mesh_m.f "sim_mesh_m.mod"
$(PWD)/sim_mesh_m.mod: $(PWD)/sim_mesh_m.o



.PHONY: clean gcov gcov-verbose constitutive_models_m.mod elems_nodes_m.mod ip_frc_stf_m.mod ip_strain_inc_m.mod kinds_m.mod lin_solver_m.mod lm_types_m.mod matrix_m.mod misc_types_m.mod post_proc_m.mod shape_fns_m.mod sim_mesh_m.mod

clean:
	find . -maxdepth 1 \( -name "*.o" -o -name "*.mod" -o -name "*.gcda" -o -name "*.gcno" \) -exec rm -f '{}' +

gcov:
	find . -maxdepth 1 \( -name "*.gcda" -o -name "*.gcno" \) -exec mv -f '{}' . 2>/dev/null \;

gcov-verbose:
	find . -maxdepth 1 \( -name "*.gcda" -o -name "*.gcno" \) -exec mv -f '{}' . \;
