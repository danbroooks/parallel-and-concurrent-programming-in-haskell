
const get = (url) => new Promise((res, rej) => {
  const xhr = new XMLHttpRequest();
  xhr.onreadystatechange = () => {
    if (xhr.readyState != 4) return;
    handleResponse(xhr).then(res).catch(rej);
  };
  xhr.open('GET', url, true);
  xhr.send();
});

const post = (url, data) => new Promise((res, rej) => {
  const xhr = new XMLHttpRequest();
  xhr.open('POST', url, true);
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.onload = () => handleResponse(xhr).then(res).catch(rej);
  xhr.send(data && JSON.stringify(data));
});

const handleResponse = xhr => new Promise((res, rej) => {
  if (xhr.status === 200) {
    res(JSON.parse(xhr.responseText));
  } else {
    rej({ error: xhr.status });
  }
});

const after = ms => new Promise((res) => setTimeout(res, ms));

const setup = div => {
  const chart = (
    div.append('div')
    .attr('class', 'chart')
  );

  const controls = (
    div.append('div')
    .attr('class', 'controls')
  );

  const resetButton = (
    controls.append('button')
    .text('Reset')
    .on('click', () => reset(api))
  );

  const updateButton = (
    controls.append('button')
    .text('Update')
    .on('click', () => update(api))
  );

  const svg = (
    chart.append('svg')
    .attr('id', 'clusters')
    .attr('width', 800)
    .attr('height', 600)
  );

  const clusters = (
    svg.append('g')
  );

  const centroids = (
    svg.append('g')
  );

  const renderCircles = (group, data) => {
    const circles =
      group.selectAll('circle').data(data);

    circles.exit().remove();

    circles
    .enter()
    .append('circle')
    .merge(circles)
    .attr('r', ({ r }) => r)
    .attr('cx', ({ x }) => x)
    .attr('cy', ({ y }) => y)
    .attr('fill', ({ fill }) => fill)
    .attr('stroke', ({ stroke }) => stroke);
  };

  const api = {
    renderClusters: data => renderCircles(clusters, data),
    renderCentroids: data => renderCircles(centroids, data),
    disableControls: () => controls.selectAll('button').attr('disabled', true),
    enableControls: () => controls.selectAll('button').attr('disabled', null),
  };

  return api;
};

const render = page => ({ title, state }) => {
  document.title = title;
  page.renderClusters(state.clusters);
  page.renderCentroids(state.centroids);
};

const update = page =>
  get('/state').then(render(page));

const reset = page => {
  page.disableControls();
  Promise.all([ post('/reset'), after(200) ])
    .then(([ data ]) => data)
    .then(render(page))
    .then(() => page.enableControls());
};

document.addEventListener('DOMContentLoaded', () =>
  update(
    setup(d3.selectAll('#kmeans'))
  )
);
