$meta->{prereqs}{runtime}{requires}{'B::Hooks::Parser'} = '0.10' if $] >= 5.008001;
$meta->{prereqs}{runtime}{requires}{'UNIVERSAL::DOES'}  = 0      if $]  < 5.010000;
