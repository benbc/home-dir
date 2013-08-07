$home = '/home/ben'

class selector {
  case $bootstrap_type {
    'minimal': {
      notice("minimal install")
      include minimal
    }
    'full': {
      notice("full install")
      include full
    }
    default: {
      fail("no install selected")
    }
  }
}

class minimal {
  include definitions
  include common

  definitions::ppa {'emacs24':
    team => 'cassou',
    ppa => 'emacs',
  }

  package {['emacs24', 'emacs24-el', 'emacs24-common-non-dfsg']:
    require => Definitions::Ppa['emacs24'],
  }

  file {"$home/.emacs.d":
    ensure => directory,
    owner => ben,
    group => ben,
  }

  file {"$home/.emacs.d/init.el":
    content => template("$home/projects/home-dir/.emacs.d/init.el"),
    owner => ben,
    group => ben,
  }

  package {['puppet-el']:
    require => Package['emacs24'],
  }
}

class full {
  include definitions
  include common
  include minimal

  package {['git', 'inotify-tools', 'xmonad', 'xmobar', 'trayer', 'rxvt-unicode',
            'suckless-tools', 'graphviz', 'vpnc', 'tree', 'powertop', 'gimp', 'exuberant-ctags', 'openssh-server',
            'vinagre', 'ruby1.9.3', 'rubygems', 'byobu', 'inkscape', 'gnuplot']:
  }

  package {['clojure']:
  }

  homedir {['projects', 'sources']: }

  project {'home-dir': }

  vcs-link {['bin', '.gitconfig', '.xmobarrc', '.Xresources', '.xsessionrc', '.bash_aliases']: }

  vcs-link {'xmonad.hs':
    dir => '.xmonad',
  }

  # Chess
  package {['scid', 'stockfish']:
  }

  # Latex
  package {['texlive', 'texlive-humanities', 'dvipng']:
  }

  # Erlang
  package {['erlang', 'erlang-manpages', 'erlang-doc']:
  }
}

class common {
  Package {
    ensure => present,
    require => [Exec['apt-get-update'], File['apt-autoremove']],
  }

  exec {'apt-get-update':
    command => '/usr/bin/apt-get update',
    refreshonly => true,
  }

  file {'apt-autoremove':
    path => '/etc/apt/apt.conf.d/always-autoremove',
    content=>'APT::Get::AutomaticRemove "true";
    APT::Get::Assume-Yes "true";',
  }
}

class definitions {
  define homedir() {
    file {"$home/$name":
      ensure => directory,
      owner => ben,
      group => ben,
    }
  }

  define repo($url, $location) {
    exec {"check-out-$name":
      command => "/usr/bin/sudo -u ben -i /usr/bin/git clone $url $location",
      creates => $location,
      require => Package['git'],
    }
  }

  define project() {
    repo {"project-$name":
      location => "$home/projects/$name",
      url => "git@github.com:benbc/$name.git",
      require => Homedir['projects'],
    }
  }

  define ppa($team, $ppa) {
    exec {"add-ppa-$name":
      command => "/usr/bin/add-apt-repository ppa:$team/$ppa",
      creates => "/etc/apt/sources.list.d/${team}-${ppa}-$lsbdistcodename.list",
      notify => Exec['apt-get-update'],
    }

    exec {"ensure-apt-get-update-is-called-before-ppa-$name-is-used":
      command => '/bin/true',
      subscribe => Exec["add-ppa-$name"],
      require => Exec['apt-get-update'],
      refreshonly => true,
    }
  }

  define vcs-link($dir=false) {
    if ($dir) {
      homedir {$dir: }
      $requires = [Project['home-dir'], Homedir[$dir]]
      $path = "$dir/$name"
    } else {
      $requires = Project['home-dir']
      $path = $name
    }

    file {"$home/$path":
      ensure => "$home/projects/home-dir/$path",
      require => $requires,
      owner => ben,
      group => ben,
    }
  }
}

include selector
