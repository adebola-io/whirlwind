import { Hono } from 'hono';

const app = new Hono();

app.get('/', (c) => {
  return c.text('Hello Hono!');
});

export default {
  port: 3100,
  fetch: app.fetch,
};
