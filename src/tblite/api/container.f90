! This file is part of tblite.
! SPDX-Identifier: LGPL-3.0-or-later
!
! tblite is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! tblite is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with tblite.  If not, see <https://www.gnu.org/licenses/>.

!> @file tblite/api/container.f90
!> Provides API exports for the #tblite_container handle.

!> API export for managing interaction containers
module tblite_api_container
   use, intrinsic :: iso_c_binding
   use mctc_env, only : wp, error_type, fatal_error
   use tblite_api_version, only : namespace
   use tblite_container, only : container_type
   use tblite_api_structure, only: vp_structure
   use tblite_external_field, only : electric_field
   use tblite_solvation, only: solvation_type, solvation_input, new_solvation
   use tblite_api_error, only : vp_error

   implicit none
   private

   public :: vp_container, delete_container_api


   !> Void pointer to a container instance
   type :: vp_container
      !> Actual container
      class(container_type), allocatable :: ptr
   end type vp_container

   logical, parameter :: debug = .false.

contains


function new_electric_field_api(efield) result(vcont) &
      & bind(C, name=namespace//"new_electric_field")
   real(c_double), intent(in) :: efield(3)
   type(c_ptr) :: vcont
   type(vp_container), pointer :: cont

   allocate(cont)
   cont%ptr = electric_field(efield)
   vcont = c_loc(cont)
end function new_electric_field_api


function new_solvation_api(vmol) result(vcont) &
      & bind(C, name=namespace//"new_solvation")
   type(c_ptr), value :: vmol
   type(vp_structure), pointer :: mol
   type(c_ptr) :: vcont
   type(vp_container), pointer :: cont
   class(solvation_type), allocatable :: solv
   type(solvation_input) :: solv_input
   type(vp_error), pointer :: error

   if (.not.c_associated(vmol)) return
   call c_f_pointer(vmol, mol)

   allocate(solv_input%alpb)
   solv_input%alpb%dielectric_const = 79.0d0
   allocate(cont)
   call new_solvation(solv, mol%ptr, solv_input, error%ptr)
   if (allocated(error%ptr)) then
      call fatal_error(error%ptr, "Unable to create new solvation")
   endif
   call move_alloc(solv, cont%ptr)
   cont%ptr = solv
   vcont = c_loc(cont)
end function new_solvation_api


subroutine delete_container_api(vcont) &
      & bind(C, name=namespace//"delete_container")
   type(c_ptr), intent(inout) :: vcont
   type(vp_container), pointer :: cont

   if (debug) print '("[Info]", 1x, a)', "delete_container"

   if (c_associated(vcont)) then
      call c_f_pointer(vcont, cont)

      deallocate(cont)
      vcont = c_null_ptr
   end if
end subroutine delete_container_api


end module tblite_api_container
