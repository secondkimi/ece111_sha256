module sha256(input logic clk, reset_n, start,
 input logic [31:0] message_addr, size, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);
 
 
 	
 enum logic [3:0] {IDLE=4'b0000, PRO_READ=4'b0001, SET_ONLY=4'b0010, READ_AND_SET=4'b0011, READ_ONLY=4'b1000, DONE=4'b0101, PADDING=4'B0110, PROT=4'b0111} state;

  logic [15:0] rc, wc; // read and write counters
  
  logic [31:0] blk_counter; // varies from 0 to (# of blocks - 1)
  logic [31:0] num_blks; // assigned by the function call 
  logic [31:0] S0, s1, maj, t1, t2, ch;
  logic [7:0] t; // t varies from 0 to 63

  assign mem_clk = clk;
  parameter int sha256_k[0:63] = '{
 32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
 32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
 32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
 32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
 32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
 32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
 32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
 32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};

logic [31:0] H0 = 32'h6a09e667;
logic [31:0] H1 = 32'hbb67ae85;
logic [31:0] H2 = 32'h3c6ef372;
logic [31:0] H3 = 32'ha54ff53a;
logic [31:0] H4 = 32'h510e527f;
logic [31:0] H5 = 32'h9b05688c;
logic [31:0] H6 = 32'h1f83d9ab;
logic [31:0] H7 = 32'h5be0cd19;

logic [31:0] A; 
logic [31:0] B; 
logic [31:0] C; 
logic [31:0] D; 
logic [31:0] E; 
logic [31:0] F; 
logic [31:0] G; 
logic [31:0] H;

logic [31:0] M[0:15];
logic is_padding_block;

int i;

function logic [15:0] determine_num_blocks(input logic [31:0] size);
    if ((size << 3) % 512 <= 447)
      determine_num_blocks = ((size << 3)/512) + 1;
    else
      determine_num_blocks = ((size << 3)/512) + 2;
endfunction


function logic [31:0] rrot(input logic[31:0] oldW, input logic[7:0] r);
begin	
	rrot = ((oldW << (32-r)) | (oldW>>r));
end
endfunction

function logic [31:0] wtnew; // function with no inputs
	logic [31:0] s0, s1;
	s0 = rrot(w[t-15],7)^rrot(w[t-15],18)^rrot(w[t-15],3);
	s1 = rrot(w[t-2],17)^rrot(w[t-2],19)^rrot(w[t-2],10);
	wtnew = w[t-16] + s0 + w[t-7] + s1;
endfunction  


  
  always_ff @(posedge clk, negedge reset_n)
  begin
    if (!reset_n) begin
      state <= IDLE;
		done <= 0;
    end else
      case (state)
      IDLE: // start
          if (start) begin 
			   // global setup goes here. Not per Block variables
				blk_counter <= 0; // increment blk_counter at the end of each M. Compare Num blks with blk_counter
				num_blks <= determine_num_blocks(size);
				state <= PRO_READ;
				is_padding_block <= 0;
          end
		PRO_READ:
			// init A to H. BLK_COUNTER should already been pointing to the right addr
			begin
				A <= H0;
				B <= H1;
				C <= H2;
				D <= H3;
				E <= H4;
				F <= H5;
				G <= H6;
				H <= H7;
				t <= 0;
				if(num_blks == blk_counter+1 && is_padding_block) begin
					for(i=0; i<14; i=i+1) begin
						M[i] = 0;
					end
					M[14] = (size>>32) & 32'hffffffff;
					M[15] = size & 32'hffffffff;	
					state <= PROT;
				end else begin
				
					rc <= 1;
					wc <= 0;
					mem_we <= 0;
					mem_addr <= message_addr+blk_counter*16;
					state <= READ_ONLY;
				end
			end
		READ_ONLY:
			begin
				mem_we <= 0;
				mem_addr <= message_addr+blk_counter*16 + rc;
				rc <= rc + 1;
				state <= READ_AND_SET;
			end
		READ_AND_SET:
			begin
				M[wc] <= mem_read_data;
				wc <= wc + 1;
				if(rc < 16) begin		
					mem_we <= 0;
					mem_addr <= message_addr+blk_counter*16 + rc;
					rc <= rc + 1;
					state <= READ_AND_SET;
				end else begin
					state <= SET_ONLY;
				end				
			end
		SET_ONLY:
			begin
				M[wc] <= mem_read_data;
				// TODO: consider when to increment blk_counter.
				/*blk_counter <= blk_counter + 1;*/
				if( (blk_counter+1)*64 <= size) begin
					// what could happen is there are two blocks left to fill.
					state <= proT;
				end else begin
					state <= PADDING;
				end
			end
		PADDING:
		begin
			// use size to make sure from which position of M should you start padding
			// TODO: the address to add one byte of one is size - blk_counter*64
			// set is_padding_block to 1 if there is one more block left
			if(blk_counter < num_blks -1) begin
				is_padding_block <= 1'b1;
			end else begin
				// TODO: fill the last two words with size
			end
		end
		PROT:
		begin
		end

      endcase
  end

endmodule


/*
module calc_num_blocks(input logic [31:0] size,
                      output logic [15:0] num_blocks);

  function logic [15:0] determine_num_blocks(input logic [31:0] size);
    if ((size << 3) % 512 <= 447)
      determine_num_blocks = ((size << 3)/512) + 1;
    else
      determine_num_blocks = ((size << 3)/512) + 2;
  endfunction

  assign num_blocks = determine_num_blocks(size);
endmodule
*/