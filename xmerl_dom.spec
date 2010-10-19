%define rpmrel _RPM_RELEASE

BuildRequires: erlang

Summary: xmerl_dom: An XMerL based DOM library
Name: xmerl_dom
Version: _RPM_VERSION
License: GPL
Release: %{rpmrel}
Requires: erlang
Group: Languages/XML
Source: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-root

%description
Authors
--------------------------
  Gregory Haskins <ghaskins@novell.com>

%debug_package
%prep
%setup

%build
make

%install
make install INSTPATH=$RPM_BUILD_ROOT%{_libdir}/erlang/lib

# Install documentation  
%clean
make clean

%files
%defattr(-,root,root)
%{_libdir}/erlang/lib/%{name}-%{version}

%changelog
