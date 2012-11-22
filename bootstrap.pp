$home = '/home/ben'

define homedir() {
  file {"$home/$name":
    ensure => directory,
    owner => ben,
    group => ben,
  }
}

define project($url) {
  $location = "$home/projects/$name"

  exec {"check-out-$name":
    command => "/usr/bin/sudo -u ben -i /usr/bin/git clone $url $location",
    creates => $location,
    require => [Package['git'], Homedir['projects']],
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
  }
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

ppa {'emacs-snapshots':
  team => 'cassou',
  ppa => 'emacs',
}

package {['puppet', 'puppet-el', 'git', 'inotify-tools', 'xmonad', 'xmobar', 'trayer', 'rxvt-unicode',
          'suckless-tools']:
  ensure => latest,
  require => [Exec['apt-get-update'], File['apt-autoremove']],
}

package {'emacs-snapshot':
  ensure => latest,
  require => Ppa['emacs-snapshots'],
}

homedir {['work', 'projects']: }

project {['home-dir']: 
  url => 'git@github.com:benbc/home-dir.git',
}

vcs-link {['bin', '.gitconfig', 'xmobarrrc', '.Xresources', '.xsessionrc']: }

vcs-link {'init.el':
  dir => '.emacs.d',
}

vcs-link {'xmonad.hs':
  dir => '.xmonad',
}

file {'/mnt/backups':
  ensure => directory,
}

mount {'/mnt/backups':
  device => '/dev/sda4',
  ensure => mounted,
  fstype => auto,
  options => defaults,
  atboot => true,
  require => File['/mnt/backups'],
}
