import Main from './output/Main';

function main() {
  const body = document.querySelector('body');
  Array.from(body.children).forEach(element => {
    body.removeChild(element);
  });
  const environment = process.env.ENVIRONMENT;
  const apiUrl = process.env.API_URL;
  Main.main(environment)(apiUrl)();
}

main();
