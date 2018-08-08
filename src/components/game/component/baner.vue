<template>

    <div class="banner">
      <p class="animated fadeIn">{{content}}</p>
    </div>
</template>


<script>
  import brg from '../../../utils/middle'
    export default {
        data() {
            return {
              show:false,
              content:'',
              timer:null
            }
        },
        mounted() {
          brg.$on('protoMsg',(obj)=>{
            let {id,msg} = obj;

            if(id==510){
              if(msg.type==1){
                this.show = true;
                this.content = msg.content
                this.timer&&clearTimeout(this.timer)

                this.timer = setTimeout(()=>{
                  this.show = false
                  this.content = ''
                },3000)

              }
            }
          })

        },
        created() {

        },
        computed: {}
    }
</script>

<style scoped lang="scss">
  .banner{
    position: fixed;
    width: 100vh;
    top: 100%;
    pointer-events: none;
    left: 10%;
    -webkit-transform-origin: 0 0;
    -moz-transform-origin: 0 0;
    -ms-transform-origin: 0 0;
    -o-transform-origin: 0 0;
    transform-origin: 0 0;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    p{
      display: inline-block;
      padding:0 .4rem;
      background: rgba(0,0,0,.6);
      font-size: .24rem;
      line-height: 2;
      border-radius: .4rem;

    }
  }
</style>
