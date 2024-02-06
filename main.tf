
terraform {

  required_providers {
    docker = {
      source = "kreuzwerker/docker"
      version = "3.0.2"
    }
  }

  backend "remote" {
    hostname = "localhost:3001/api"
    organization = "harness"

    workspaces {
      name = "my-workspace"
    }
  }

}

provider "docker" {}

resource "docker_image" "nginx" {
  name = "nginx:latest"
}

resource "docker_container" "nginx" {
  image   = docker_image.nginx.image_id
  name    = "nginx"

  ports {
    external = 8080
    internal = 80
  }

}

resource "docker_container" "postgres" {
  image = ""
  name  = ""

  env = []
}
